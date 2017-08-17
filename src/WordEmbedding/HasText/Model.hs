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
  , genNoiseDistribution
  ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Mutable
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

initLParams :: Double -> Int -> RM.GenIO -> IO LParams
initLParams initLR dim gR = do
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
  join . liftIO . foldM (sampleNegative nDist rand) samplePositive $ [1 .. negs]
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

getNegative :: RMC.CondensedTableV Entry -> RM.GenIO -> T.Text -> IO Entry
getNegative noiseTable rand input = tryLoop
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
