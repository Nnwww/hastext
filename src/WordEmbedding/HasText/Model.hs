{-# LANGUAGE BangPatterns #-}
module WordEmbedding.HasText.Model where

import qualified WordEmbedding.HasText.Args      as HA
import qualified WordEmbedding.HasText.Dict      as HD

import qualified Data.Text                        as T
import qualified Data.HashMap.Strict              as HS
import qualified Data.Array.Unboxed               as AU
import qualified Data.Vector                      as V
import qualified Data.List                        as L
import qualified Data.Bifunctor                   as DB

import qualified Numeric.LinearAlgebra            as LA
import qualified Numeric.LinearAlgebra.Devel      as LAD
import qualified System.Random.MWC                as RM
import qualified System.Random.MWC.CondensedTable as RMC

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

data Params = Params
  { args          :: HA.Args
  , dict          :: HD.Dict
  , sigf          :: Double -> Double
  , logf          :: Double -> Double
  , noiseDist     :: RMC.CondensedTableV HD.Entry
  , wordVecRef    :: WordVecRef
  , tokenCountRef :: MVar Word
  }

data Model = Model
  { localTokens     :: !Word
  , learningRate    :: !Double
  , loss            :: !Double
  , hiddenL         ::  LA.Vector Double
  , gradVec         ::  LA.Vector Double
  , gRand           :: !RM.GenIO
  }

type WordVec    = HD.TMap Weights
type WordVecRef = MVar WordVec

-- | The pair of input/output word vectors correspond to a word.
data Weights = Weights
  { wI :: LA.Vector Double -- ^ input word vector
  , wO :: LA.Vector Double -- ^ output word vector
  }

initModel :: Int -> RM.GenIO -> Model
initModel dim gR = Model
  { localTokens = 0
  , learningRate = 0.0
  , loss = 0.0
  , hiddenL = zeros
  , gradVec = zeros
  , gRand = gR
  }
  where
    zeros = LA.fromList $ replicate dim 0.0
{-# INLINE initModel #-}

lookE hs k = hs HS.! k
{-# INLINE lookE #-}

inverse :: (Integral i, Fractional r) => i -> r
inverse d = 1.0 / (fromIntegral d)
{-# INLINE inverse #-}

computeHidden :: WordVecRef -> V.Vector T.Text -> IO (LA.Vector Double)
computeHidden wsRef input = do
  ws <- readMVar wsRef
  let sumVectors = V.foldl1 (+) . V.map (getWI ws) $ input
  return $ (inverse . V.length $ input) `LA.scale` sumVectors
  where
    getWI w = wI . lookE w
{-# INLINE computeHidden #-}

type RandomIO   = IO
type MVarIO   = IO

-- |
-- The function that update model based on formulas of the objective function and binary label.
binaryLogistic :: Double -- ^ learning late
               -> Bool   -- ^ label in Xin's tech report. (If this is True function compute about positive word. If False, negative-sampled word.)
               -> T.Text -- ^ a updating target word
               -> ReaderT Params (StateT Model MVarIO) ()
binaryLogistic lr label input = do
  Params{wordVecRef = wvRef, sigf = sigt, logf = logt}     <- ask
  model@Model{loss = ls, hiddenL = hidden, gradVec = grad} <- lift get
  ws <- liftIO $ takeMVar wvRef
  let wo         = wO   $ lookE ws input
      score      = sigt $ LA.dot wo hidden
      alpha      = lr * (boolToNum label - score)
      newWO      = wo + LA.scale alpha hidden
      updateWO w = Just $ w{wO = newWO}
      newWs      = HS.update updateWO input ws
  liftIO $ putMVar wvRef newWs
  let minusLog   = if label then negate $ logt score else negate $ logt (1.0 - score)
      newGrad    = grad + (LA.scale alpha wo)
      newLoss    = minusLog + ls
  lift . put $ model{loss = newLoss, gradVec = newGrad}
  where
    boolToNum = fromIntegral . fromEnum
{-# INLINE binaryLogistic #-}

-- |
-- Negative-sampling function, one of the word2vec's efficiency optimization tricks.
negativeSampling :: Double   -- ^ learning rate
                 -> T.Text   -- ^ a updating target word
                 -> ReaderT Params (StateT Model MVarIO) ()
negativeSampling lr input = do
  genRand  <- lift $ gets gRand
  nDst <- asks noiseDist
  negs <- asks $ HA.negatives . snd . args
  processForBinLogs <- liftIO $ foldM (sampleNegative nDst genRand) samplePositive [0 .. negs]
  processForBinLogs
  where
    binLog = binaryLogistic lr
    samplePositive = binLog True input
    sampleNegative :: RMC.CondensedTableV HD.Entry -> RM.GenIO
                   -> ReaderT Params (StateT Model MVarIO) () -> Word
                   -> RandomIO (ReaderT Params (StateT Model MVarIO) ())
    sampleNegative noise rand acc _ = do
      HD.Entry{HD.eword = negWord} <- getNegative noise rand input
      return (acc >> binLog False negWord)

-- update :: Model -> V.Vector T.Text -> T.Text -> Double -> IO Model
-- update model inputs updTarget lr = do
--   newH <- computeHidden (wordVecRef model) inputs
--   m <- negativeSampling model{hiddenL = newH} lr updTarget
--   let wIPlusGrad ws k = HS.update (\w -> Just w{wI = (+ gradVec m) $ wI w}) k ws
--   let wvRef = wordVecRef m
--   ws <- takeMVar wvRef
--   putMVar wvRef $ V.foldl' wIPlusGrad ws inputs
--   return $ m{wordVecRef = wvRef}

-- |
-- The function that update a model. This function is a entry point of Model module.
update :: V.Vector T.Text -> T.Text -> Double -> ReaderT Params (StateT Model MVarIO) ()
update inputs updTarget lr = do
  wvRef <- asks wordVecRef
  newHidden <- liftIO $ computeHidden wvRef inputs
  lift . modify $ updateHidden newHidden
  negativeSampling lr updTarget
  grad <- lift $ gets gradVec
  liftIO . modifyMVar_ wvRef $ return . updateWordVecs grad
  where
    updateHidden h m = m{hiddenL = h}
    updateWordVecs grad ws = V.foldl' (wIPlusGrad grad) ws inputs
    wIPlusGrad grad ws k = HS.update (\w -> Just w{wI = grad + wI w}) k ws

getNegative :: RMC.CondensedTableV HD.Entry -> RM.GenIO -> T.Text -> RandomIO HD.Entry
getNegative noiseTable rand input = tryLoop
  where
    tryLoop = do
      ent <- RMC.genFromTable noiseTable rand
      if HD.eword ent == input then tryLoop else return ent
{-# INLINE getNegative #-}

genNoiseDistribution :: Double                       -- ^ nth power of unigram distribution
                     -> HD.TMap HD.Entry             -- ^ vocabulary set for constructing a noise distribution table
                     -> RMC.CondensedTableV HD.Entry -- ^ noise distribution table
genNoiseDistribution power ents =
  RMC.tableFromProbabilities . V.map (DB.second divZ) . V.fromList $ countToPowers
  where
    -- Z is a normalization parameter of the noise distribution in paper.
    divZ a = a / z
    z = L.sum . L.map snd $ countToPowers
    countToPowers = HS.elems . HS.map (\ent -> (ent, countToPower ent)) $ ents
    countToPower ent = (fromIntegral . HD.count $ ent) ** power

genHierarchical :: HD.TMap HD.Entry -- ^ vocabulary set for building a hierarchical softmax tree
                -> Double           -- ^ learning rate
                -> T.Text           -- ^ a input word
                -> Double           -- ^ loss parameter
genHierarchical ents lr input = undefined

-- | generate memorized sigmoid function.
genSigmoid :: Word   -- ^ table size
           -> Double -- ^ the maximum value of x axis
           -> (Double -> Double)
genSigmoid tableSize maxValue x
  | x < -maxValue = 0.0
  | maxValue < x  = 1.0
  | otherwise     = sigmoidTable AU.! (mapInputToIndex x)
  -- TODO: using closures will be less efficieny than inlined functions. Therefore there's room for consideration.
  where
    doubledTableSize = fromIntegral tableSize :: Double

    mapInputToIndex :: Double -> Word
    mapInputToIndex lx = floor $ ((lx + maxValue) * doubledTableSize / maxValue / 2.0)

    sigmoidTable :: AU.UArray Word Double
    sigmoidTable = AU.listArray (0, tableSize)
      [lsigmoid . mapIndexToTableX $ i | i <- [0.0 .. doubledTableSize]]

    mapIndexToTableX :: Double -> Double
    mapIndexToTableX idx = (idx * 2.0 * maxValue) / doubledTableSize - maxValue
    lsigmoid lx = 1.0 / (1.0 + (exp $ -lx))

-- | generate memorized log function.
genLog :: Word -> (Double -> Double)
genLog tableSize x
  | 1.0 < x   = 0.0
  | otherwise = logTable AU.! (mapInputToIndex x)
  where
    doubledTableSize = fromIntegral tableSize :: Double

    mapInputToIndex :: Double -> Word
    mapInputToIndex lx = floor $ (lx * doubledTableSize)

    logTable :: AU.UArray Word Double
    logTable = AU.listArray (0, tableSize)
      [Prelude.log $ ((i + 1e-5) / doubledTableSize) | i <- [0.0 .. doubledTableSize]]
      -- I add 1e-5 to x due to avoid computing log 0.
