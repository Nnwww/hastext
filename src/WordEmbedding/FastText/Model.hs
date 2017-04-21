module WordEmbedding.FastText.Model where

import           WordEmbedding.FastText.Args
import qualified Data.Array.Unboxed as AU

-- memorized sigmoid function.
sigmoid :: Word -> Double -> (Double -> Double)
sigmoid tableSize maxValue x =
  if x < -maxValue     then 0.0
  else if maxValue < x then 1.0
  else sigmoidTable AU.! (mapInputToTable x)
  where
    doubledTableSize = fromIntegral tableSize :: Double

    mapInputToTable :: Double -> Word
    mapInputToTable x' = round $ ((x' + maxValue) * doubledTableSize / maxValue / 2.0)

    sigmoidTable :: AU.UArray Word Double
    sigmoidTable = AU.listArray (0, tableSize)
      [lsigmoid . mapIndexToTable $ i | i <- [0.0 .. doubledTableSize]]

    mapIndexToTable idx = (idx * 2.0 * maxValue) / doubledTableSize - maxValue
    lsigmoid lx = 1.0 / (1.0 + (exp $ -lx))
