{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module WordEmbedding.HasText.Internal.Strict.MVectorOps where

import qualified Data.Vector.Unboxed.Mutable      as VUM
import qualified Data.Vector.Unboxed              as VU


scale :: Double -> VUM.IOVector Double -> IO ()
scale alpha v = mapi (\_ e -> alpha * e) v

add :: VUM.IOVector Double -> VU.Vector Double -> IO ()
add ov iv = mapi (\i e -> e + VU.unsafeIndex iv i) ov

addMM :: VUM.IOVector Double -> VUM.IOVector Double -> IO ()
addMM ov iv = foriM_ ov $ \i e -> do
  eiv <- VUM.unsafeRead iv i
  pure (eiv + e)

mapi :: (Int -> Double -> Double) -> VUM.IOVector Double -> IO ()
mapi f ov = go 0
  where
    len = VUM.length ov
    go i
      | i >= len = pure ()
      | len > i  = do
          VUM.unsafeModify ov (f i) i
          go (i + 1)

mapiM_ :: (Int -> Double -> IO Double) -> VUM.IOVector Double -> IO ()
mapiM_ f ov = go 0
  where
    len = VUM.length ov
    go i
      | i >= len = pure ()
      | len > i  = do
          e <- VUM.unsafeRead ov i
          res <- f i e
          VUM.unsafeWrite ov i res
          go (i + 1)

foriM_ :: VUM.IOVector Double -> (Int -> Double -> IO Double) -> IO ()
foriM_ ov f = mapiM_ f ov
{-# INLINE foriM_ #-}

foldiM :: VUM.IOVector Double -> a -> (Int -> a -> Double -> IO a) -> IO a
foldiM ov a f = go 0 a
  where
    len = VUM.length ov
    go i acc
      | i >= len = pure acc
      | len > i  = do
          e <- VUM.unsafeRead ov i
          res <- f i acc e
          go (i + 1) res

sumDotMM :: VUM.IOVector Double -> VUM.IOVector Double -> IO Double
sumDotMM av bv = foldiM av 0 $ \i acc e -> do
  ebv <- VUM.unsafeRead bv i
  return (acc + e * ebv)
