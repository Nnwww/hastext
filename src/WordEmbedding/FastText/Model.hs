module WordEmbedding.FastText.Model where

import qualified WordEmbedding.FastText.Args      as FA
import qualified WordEmbedding.FastText.Dict      as FD

import qualified Data.Text                        as T
import qualified Data.HashMap.Strict              as HS
import qualified Data.Array.Unboxed               as AU
import qualified Data.Vector.Generic              as VG
import qualified Data.Foldable                    as F
import qualified Data.List                        as L
import qualified Data.Monoid                      as M

import qualified Numeric.LinearAlgebra            as LA
import qualified Numeric.LinearAlgebra.Devel      as LAD
import qualified System.Random.MWC                as RM
import qualified System.Random.MWC.CondensedTable as RMC

import Control.Monad
import Control.Monad.ST

data Model s = Model
  { weightI   :: LAD.STMatrix s Double
  , weightO   :: LAD.STMatrix s Double
  , dict      :: FD.Dict
  , loss      :: Double
  , hiddenL   :: LA.Vector Double
  , outputL   :: LA.Vector Double
  , grad      :: LA.Vector Double
  , sigf      :: Double -> Double
  , logf      :: Double -> Double
  , gRand     :: RM.GenST s
  , negatives :: LA.Vector Double
  , negpos    :: Word
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

genNegatives :: Double           -- ^ nth power of unigram distribution
             -> FD.TMap FD.Entry -- ^ vocabulary set for constructing a noise distribution table
             -> (Double          -- ^ learning rate
             ->  RM.GenST s      -- ^ a PRNG state in the ST monad
             ->  T.Text          -- ^ a input word
             ->  ST s FD.Entry)    -- ^ loss parameter
genNegatives power ents lr gen input = RMC.genFromTable noiseDist gen
  where
    noiseDist :: RMC.CondensedTableV FD.Entry
    noiseDist = RMC.tableFromProbabilities . VG.map (\(ent, ctp) -> (ent, ctp / z)) . VG.fromList $ countToPowers
    -- Z is a normalization parameter of the noise distribution in paper.
    z = L.sum . L.map snd $ countToPowers
    countToPowers = HS.elems . HS.map (\ent -> (ent, countToPower ent)) $ ents
    countToPower ent = (fromIntegral . FD.count $ ent) ** power

genHierarchical :: FD.TMap FD.Entry -- ^ vocabulary set for building a hierarchical softmax tree
                -> (Double       -- ^ learning rate
                ->  T.Text       -- ^ a input word
                ->  Double)      -- ^ loss parameter
genHierarchical ents lr input = undefined

-- | generate memorized sigmoid function.
genSigmoid :: Word -> Double -> (Double -> Double)
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
