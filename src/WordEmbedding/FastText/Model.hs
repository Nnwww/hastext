{-# LANGUAGE BangPatterns #-}
module WordEmbedding.FastText.Model where

import qualified WordEmbedding.FastText.Args      as FA
import qualified WordEmbedding.FastText.Dict      as FD

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
import Control.Monad.ST
import Control.Monad.State

data Model s = Model
  { args      :: FA.Args
  , weights   :: MVar (FD.TMap Weights)
  , dict      :: FD.Dict
  , loss      :: Double
  , hiddenL   :: LA.Vector Double
  , gradV     :: LA.Vector Double
  , sigf      :: Double -> Double
  , logf      :: Double -> Double
  , noiseDist :: RMC.CondensedTableV FD.Entry
  , gRand     :: RM.GenIO
  }

-- | The pair of input/output word vectors correspond to a word.
data Weights = Weights
  { wI :: LA.Vector Double -- ^ input word vector
  , wO :: LA.Vector Double -- ^ output word vector
  }
-- genLossFunction :: FA.Args -> FD.Dict -> (Double -> T.Text -> Double)
-- -- genLossFunction (_, FA.Options{loss = los}) (FD.Dict{FD.entries = ents}) lr input =
-- genLossFunction args dic lr input =
--   case los of
--     FA.Negative     -> genNegatives 0.75 ents lr input
--     FA.Hierarchical -> genHierarchical ents lr input
--   where
--     ents = FD.entries dic
--     los  = FA.loss . snd $ args


lookE hs k = hs HS.! k
{-# INLINE lookE #-}

inverse :: (Integral i, Fractional r) => i -> r
inverse d = 1.0 / (fromIntegral d)
{-# INLINE inverse #-}

computeHidden :: MVar (FD.TMap Weights) -> V.Vector T.Text -> IO (LA.Vector Double)
computeHidden wghs input = do
  ws <- readMVar wghs
  let sumVectors = V.foldl1 (+) . V.map (getWI ws) $ input
  return $ (inverse . V.length $ input) `LA.scale` sumVectors
  where
    getWI wghs = wI . lookE wghs
{-# INLINE computeHidden #-}

-- |
-- The function that update model based on formulas of the objective function and binary label.
-- This function take a strategy using State monad to keep updating models.
binaryLogistic :: Double -- ^ learning late
               -> Bool   -- ^ label in Xin's tech report. (If this is True function compute about positive word. If False, negative-sampled word.)
               -> T.Text -- ^ a updating target word
               -> State (Model s) ()
binaryLogistic lr label input = do
  model <- get
  let ws         = weights model
      hidden     = hiddenL model
      sigt       = sigf model
      logt       = logf model
      wo         = wO $ lookE ws input
      score      = sigt $ LA.dot wo hidden
      alpha      = lr * ((if label then 1 else 0) - score)
      newWO      = wo + (LA.scale alpha hidden)
      updateWO w = Just $ w{wO = newWO}
      minusLog   = if label then -(logt score) else -(logt $ (1.0 - score))
      !newGrad   = (gradV model) + (LA.scale alpha wo)
      !newLoss   = minusLog + loss model
      !newWs     = HS.update updateWO input ws
  put $ model{loss = newLoss, gradV = newGrad, weights = newWs}

-- |
-- Negative-sampling function, one of the word2vec's efficiency optimization tricks.
negativeSampling :: Model s
                 -> Double         -- ^ learning rate
                 -> T.Text         -- ^ a updating target word
                 -> ST s (Model s) -- ^ ST monad is used for random generater in Models.
negativeSampling model lr input = do
  processForBinLogs <- foldM sampleNegative samplePositive [0 .. negs]
  return $ execState processForBinLogs model
  where
    negs         = FA.negatives . snd . args $ model
    presetBinLog = binaryLogistic lr
    samplePositive     = presetBinLog True input
    sampleNegative _ _ =
      (return . presetBinLog False . FD.eword) =<< getNegative model input

-- |
-- The function that update a model. This function is a entry point of Model module.
update :: Model s -> V.Vector T.Text -> T.Text -> Double -> ST s (Model s)
update model inputs updTarget lr = do
  let newH = computeHidden (weights model) inputs
  m <- negativeSampling model{hiddenL = newH} lr updTarget
  let wIPlusGrad ws k = HS.update (\w -> Just w{wI = (+ gradV m) . wI $ w}) k ws
  return $ m{weights = V.foldl' wIPlusGrad (weights m) inputs}

getNegative :: Model s -> T.Text -> ST s FD.Entry
getNegative model input = tryLoop
  where
    tryLoop = do
      ent <- RMC.genFromTable noise rand
      if FD.eword ent == input then tryLoop else return ent
    noise = noiseDist model
    rand = gRand model

genNoiseDistribution :: Double                       -- ^ nth power of unigram distribution
                     -> FD.TMap FD.Entry             -- ^ vocabulary set for constructing a noise distribution table
                     -> RMC.CondensedTableV FD.Entry -- ^ noise distribution table
genNoiseDistribution power ents =
  RMC.tableFromProbabilities . V.map (DB.second divZ) . V.fromList $ countToPowers
  where
    -- Z is a normalization parameter of the noise distribution in paper.
    divZ a = a / z
    z = L.sum . L.map snd $ countToPowers
    countToPowers = HS.elems . HS.map (\ent -> (ent, countToPower ent)) $ ents
    countToPower ent = (fromIntegral . FD.count $ ent) ** power

genHierarchical :: FD.TMap FD.Entry -- ^ vocabulary set for building a hierarchical softmax tree
                -> Double          -- ^ learning rate
                -> T.Text          -- ^ a input word
                -> Double         -- ^ loss parameter
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

-- | The function shuffling @Numeric.LinearAlgebra.Vector@ uniformity.
uniformShuffle :: LA.Vector Double -> RM.GenST s -> ST s (LA.Vector Double)
uniformShuffle vec gen
  | size <= 1 = return vec
  | otherwise = do
      stVec <- LAD.thawVector vec
      uniformShuffleST stVec size gen
      LAD.unsafeFreezeVector stVec
  where
    size = LA.size vec

{-# INLINE uniformShuffle #-}

-- | The ST monad action generating sub-effect that shuffle @Numeric.LinearAlgebra.Vector@ using Fisher-Yates algorithm.
uniformShuffleST :: LAD.STVector s Double -> Int -> RM.GenST s -> ST s ()
uniformShuffleST stVec size gen
  | size <= 1 = return ()
  | otherwise = do
      forM_ [0 .. size - 2] $ \i -> do
        j  <- RM.uniformR (i, size - 1) gen
        ei <- LAD.unsafeReadVector stVec i
        LAD.unsafeWriteVector stVec i =<< LAD.unsafeReadVector stVec j
        LAD.unsafeWriteVector stVec j ei

{-# INLINE uniformShuffleST #-}
