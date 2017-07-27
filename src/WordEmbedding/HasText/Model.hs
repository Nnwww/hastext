{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE IncoherentInstances #-}

module WordEmbedding.HasText.Model
  ( Params(..)
  , LParams(..)
  , Model
  , WordVec
  , WordVecRef
  , Weights(..)
  , MWeights(..)
  , initLParams
  , updateModel
  , genSigmoid
  , genLog
  , genNoiseDistribution
  ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Mutable
import qualified Data.Array.Unboxed                       as AU
import qualified Data.HashMap.Strict                      as HS
import qualified Data.List                                as L
import qualified Data.Text                                as T
import qualified Data.Vector                              as V
import qualified Data.Vector.Unboxed.Mutable              as VUM
import           Data.Binary                              (Binary)
import           Data.Binary.Orphans                      ()
import qualified System.Random.MWC                        as RM
import qualified System.Random.MWC.CondensedTable         as RMC
import           WordEmbedding.HasText.Dict
import           WordEmbedding.HasText.Internal.Strict.Model
import qualified WordEmbedding.HasText.Internal.Strict.MVectorOps as HMV
import           WordEmbedding.HasText.Internal.Type
                 ( HasTextOptions(..)
                 , Params(..)
                 , LParams(..)
                 , Model
                 , WordVec
                 , WordVecRef
                 , Weights(..)
                 , MWeights(..)
                 )

instance Binary Weights

initLParams :: MonadIO m => Double -> Int -> RM.GenIO -> m LParams
initLParams initLR dim gR = liftIO $ do
  iouRef <- newRef (0 :: Double)
  h <- VUM.replicate dim 0.0
  g <- VUM.replicate dim 0.0
  return LParams {_loss = iouRef, _lr = initLR, _hidden = h, _grad = g, _rand = gR}

-- |
-- Negative-sampling function, one of the word2vec's efficiency optimization tricks.
negativeSampling :: T.Text -- ^ a updating target word
                 -> Model
negativeSampling input = do
  (Params{_args = (_, HasTextOptions{_negatives = negs}), _noiseDist = nDist},
   LParams{_rand = rand}) <- ask
  join . liftIO . foldM (sampleNegative nDist rand) samplePositive $! [1 .. negs]
    where
      samplePositive = binaryLogistic True input
      sampleNegative noise rand acc _ = do
        Entry{_eWord = negWord} <- getNegative noise rand input
        return (acc >> binaryLogistic False negWord)

-- |
-- The function that update a model. This function is a entry point of LParams module.
updateModel :: V.Vector T.Text -> T.Text -> Model
updateModel inputs updTarget = do
  (Params{_wordVecRef = wvRef}, LParams{_hidden = h, _grad = g}) <- ask
  computeHidden h wvRef inputs
  negativeSampling updTarget
  liftIO . modifyMVar_ wvRef $ \ws -> updateWordVecs g ws >> return ws
  where
    updateWordVecs grad ws = V.mapM_ (addGradmWI grad ws) inputs
    addGradmWI grad ws k = getmWI ws k `HMV.addMM` grad

getNegative :: MonadIO m => RMC.CondensedTableV Entry -> RM.GenIO -> T.Text -> m Entry
getNegative noiseTable rand input = liftIO tryLoop
  where
    tryLoop = do
      ent <- RMC.genFromTable noiseTable rand
      if _eWord ent /= input then return ent else tryLoop

genNoiseDistribution :: Double                    -- ^ nth power of unigram distribution
                     -> TMap Entry                -- ^ vocabulary set for constructing a noise distribution table
                     -> RMC.CondensedTableV Entry -- ^ noise distribution table
genNoiseDistribution power ents =
  RMC.tableFromProbabilities . V.map (second divZ) . V.fromList $ countToPowers
  where
    -- Z is a normalization parameter of the noise distribution in paper.
    divZ a = a / z
    z = L.sum . L.map snd $ countToPowers
    countToPowers = HS.elems . HS.map (id &&& countToPower) $ ents
    countToPower ent = (fromIntegral . _eCount $ ent) ** power

genHierarchical :: TMap Entry -- ^ vocabulary set for building a hierarchical softmax tree
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
  | otherwise     = sigmoidTable AU.! mapInputToIndex x
  where
    doubledTableSize = fromIntegral tableSize :: Double

    mapInputToIndex :: Double -> Word
    mapInputToIndex lx = floor ((lx + maxValue) * doubledTableSize / maxValue / 2.0)

    sigmoidTable :: AU.UArray Word Double
    sigmoidTable = AU.listArray (0, tableSize)
      [lsigmoid . mapIndexToTableX $ i | i <- [0.0 .. doubledTableSize]]

    mapIndexToTableX :: Double -> Double
    mapIndexToTableX idx = (idx * 2.0 * maxValue) / doubledTableSize - maxValue
    lsigmoid lx = 1.0 / (1.0 + exp (negate lx))

-- | generate memorized log function.
genLog :: Word -> (Double -> Double)
genLog tableSize x
  | 1.0 < x   = 0.0 -- Because this function is passed probabilities.
  | otherwise = logTable AU.! mapInputToIndex x
  where
    doubledTableSize = fromIntegral tableSize :: Double

    mapInputToIndex :: Double -> Word
    mapInputToIndex lx = floor (lx * doubledTableSize)

    logTable :: AU.UArray Word Double
    logTable = AU.listArray (0, tableSize)
      [Prelude.log ((i + 1e-5) / doubledTableSize) | i <- [0.0 .. doubledTableSize]]
      -- I add 1e-5 to x due to avoid computing log 0.
