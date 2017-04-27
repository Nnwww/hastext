{-# LANGUAGE ViewPatterns #-}
module WordEmbedding.FastText.Model where

import qualified WordEmbedding.FastText.Args as FA
import qualified WordEmbedding.FastText.Dict as FD

import qualified Data.Text                   as T
import qualified Data.Map.Strict             as MS
import qualified Data.Array.Unboxed          as AU

import qualified Numeric.LinearAlgebra       as LA
import qualified Numeric.LinearAlgebra.Devel as LAD
import qualified System.Random.MWC           as RM

import Control.Monad
import Control.Monad.ST

data Model s = Model
  { wi     :: LAD.STMatrix s Double
  , wo     :: LAD.STMatrix s Double
  , dict   :: FD.Dict
  , loss   :: Double
  , hidden :: LA.Vector Double
  , sig    :: Double -> Double
  , log    :: Double -> Double
  , rand   :: RM.GenST s
  , negs   :: LA.Vector Double
  , negpos :: Word
  }

genLossFunction :: FA.Args -> FD.Dict -> (Double -> T.Text -> Double)
genLossFunction (_, FA.Options{loss = los}) (FD.Dict{FD.entries = ents}) lr input =
  case los of
    FA.Negative     -> genNegatives ents lr input
    FA.Hierarchical -> genHierarchical ents lr input

genNegatives :: FD.TMap FD.Entry -- ^ vocabulary set for constructing a negative sampling table
             -> (Double       -- ^ learning rate
             ->  T.Text       -- ^ a input word
             ->  Double)      -- ^ loss parameter
genNegatives ents lr input = undefined


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
