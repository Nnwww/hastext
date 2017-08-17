{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}

module WordEmbedding.HasText.Internal.Strict.HasText
  ( unsafeWindowRange
  , initMW
  , unsafeFreezeMW
  , norm2
  , scale
  , sigmoid
  , addUU
  , dotUU
  , unitVector
  , cosSim
  ) where

import qualified Data.Vector                         as V
import qualified Data.Vector.Unboxed                 as VU
import qualified Data.Vector.Unboxed.Mutable         as VUM
import qualified Data.Text                           as T
import qualified System.Random.MWC                   as RM
import           WordEmbedding.HasText.Internal.Type

-- The function that return a range of the dynamic window.
unsafeWindowRange :: HasTextArgs -> LParams -> V.Vector T.Text
                  -> Int -- ^ The central index of a window. Note that no boundary checks.
                  -> IO (V.Vector T.Text)
unsafeWindowRange a lp line targetIdx = do
  winRange <- RM.uniformR (0, negs) . _rand $ lp
  let winFrom = if targetIdx - winRange > 0 then targetIdx - winRange else 0
      winTo   = if V.length line > targetIdx + winRange then targetIdx + winRange else V.length line - 1
      inWindowAndNotTarget i _ = winFrom < i && i < winTo && i /= targetIdx
  return $ V.ifilter (\i e -> not $ inWindowAndNotTarget i e) line
  where
    negs = fromIntegral . _negatives . snd $ a

initMW :: RM.GenIO -> Int -> IO MWeights
initMW rnd dm = do
  randoms <- VUM.replicateM dm $ RM.uniformR range rnd
  zeros   <- VUM.new dm
  return MWeights{_mwI = randoms, _mwO = zeros}
  where
    range :: (Double, Double)
    range = (-1 / fromIntegral dm, 1 / fromIntegral dm)

unsafeFreezeMW :: MWeights -> IO Weights
unsafeFreezeMW MWeights{..} = do
  wi <- VU.unsafeFreeze _mwI
  wo <- VU.unsafeFreeze _mwO
  return Weights{_wI = wi, _wO = wo}

norm2 :: VU.Vector Double -> Double
norm2 v = sqrt . VU.foldl1 (+) . VU.map (**2) $ v

scale :: Double -> VU.Vector Double -> VU.Vector Double
scale coeff v = VU.map (coeff *) v

addUU :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
addUU = VU.zipWith (+)

dotUU :: VU.Vector Double -> VU.Vector Double -> Double
dotUU v1 v2 = VU.foldl1 (+) $ VU.zipWith (*) v1 v2

unitVector :: VU.Vector Double -> VU.Vector Double
unitVector v = scale (1 / norm2 v) v

cosSim :: VU.Vector Double -> VU.Vector Double -> Double
cosSim nume deno = dotUU (unitVector nume) (unitVector deno)

sigmoid :: Double -> Double
sigmoid lx = 1.0 / (1.0 + exp (negate lx))
