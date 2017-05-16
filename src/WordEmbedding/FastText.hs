{-# LANGUAGE ScopedTypeVariables #-}
module WordEmbedding.FastText where

import qualified WordEmbedding.FastText.Args  as FA
import qualified WordEmbedding.FastText.Dict  as FD
import qualified WordEmbedding.FastText.Model as FM
import qualified Data.Vector                  as V
import qualified Data.Text                    as T

import qualified System.Random.MWC                as RM

import Control.Monad.ST


skipGram :: forall s. FM.Model s -> Double -> V.Vector T.Text -> ST s (FM.Model s)
skipGram model lr line = V.ifoldM slideWindow model line
  where
    windowRange = RM.uniformR (0, negs) . FM.gRand $ model
    negs = fromIntegral . FA.negatives . snd . FM.args $ model

    slideWindow :: FM.Model s -> Int -> T.Text -> ST s (FM.Model s)
    slideWindow m targetIdx updTarget = do
      win <- windowRange
      let winFrom = if targetIdx - win > 0 then targetIdx - win else 0
          winTo   = if V.length line > targetIdx + win then targetIdx + win else V.length line - 1
      V.foldM (\acc e -> FM.update acc (V.singleton updTarget) e lr) m $ V.slice winFrom winTo line

cbow :: forall s. FM.Model s -> Double -> V.Vector T.Text -> ST s (FM.Model s)
cbow model lr line = V.ifoldM slideWindow
  where
    windowRange = RM.uniformR (0, negs) . FM.gRand $ model
    negs = fromIntegral . FA.negatives . snd . FM.args $ model

    slideWindow :: FM.Model s -> Int -> T.Text -> ST s (FM.Model s)
    slideWindow m targetIdx updTarget = do
      win <- windowRange
      let winFrom = if targetIdx - win > 0 then targetIdx - win else 0
          winTo   = if V.length line > targetIdx + win then targetIdx + win else V.length line - 1
      V.foldM (\acc e -> FM.update acc (V.singleton updTarget) e lr) m $ V.slice winFrom winTo line


-- TODO: write test code using simpler corpuses, and try to compare hastext's result with gensim's result.
--      (corpus e.g. a a a a ... b b b b ... c c c c ... d d d d ...)
-- also try to compute outsoftmax.
