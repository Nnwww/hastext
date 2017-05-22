module WordEmbedding.FastText where

import qualified WordEmbedding.FastText.Args  as FA
import qualified WordEmbedding.FastText.Dict  as FD
import qualified WordEmbedding.FastText.Model as FM
import qualified Data.Vector                  as V
import qualified Data.Text                    as T
import qualified Data.Word                    as W
import qualified Numeric.LinearAlgebra        as LA
import qualified System.Random.MWC            as RM
import qualified System.IO                    as SI

import Control.Concurrent
import Control.Monad.ST

-- The function that return a range of the dynamic window.
windowRange :: V.Vector T.Text -> FM.Model -> Int -> IO (V.Vector T.Text)
windowRange line model targetIdx = do
  winRange <- RM.uniformR (0, negs) . FM.gRand $ model
  let winFrom = if targetIdx - winRange > 0 then targetIdx - winRange else 0
      winTo   = if V.length line > targetIdx + winRange then targetIdx + winRange else V.length line - 1
      inWindowAndNotTarget i _ = winFrom < i && i < winTo && i /= targetIdx
  return $ V.ifilter inWindowAndNotTarget line
  where
    negs = fromIntegral . FA.negatives . snd . FM.args $ model

skipGram :: FM.Model -> Double -> V.Vector T.Text -> IO FM.Model
skipGram model lr line = V.ifoldM updateEachWinElems model line
  where
    updateEachWinElems m targetIdx updTarget = do
      updateRange <- windowRange line m targetIdx
      V.foldM (\acc e -> FM.update acc (V.singleton updTarget) e lr) m updateRange


cbow :: FM.Model -> Double -> V.Vector T.Text -> IO FM.Model
cbow model lr line = V.ifoldM update model line
  where
    update m targetIdx updTarget = do
      inputRange <- windowRange line m targetIdx
      FM.update m inputRange updTarget lr

-- TODO: compare parallelization using MVar with one using ParIO.
trainThread :: FA.Args -> FM.Model -> MVar W.Word64 -> Integer -> IO ()
trainThread args model tokenCountRef threadNo = do
  h <- SI.openFile inputPath SI.ReadMode
  size <- SI.hFileSize h
  SI.hSeek h SI.AbsoluteSeek (size * threadNo `quot` threads)

  SI.hClose h
  where
    inputPath = FA.input . snd $ args
    threads = fromIntegral . FA.threads . snd $ args
    ntokens = FD.ntokens . FM.dict $ model

-- TODO: write test code using simpler corpuses, and then try to compare hastext's result with gensim's result.
--      (corpus e.g. a a a a ... b b b b ... c c c c ... d d d d ...)
-- also try to compute outsoftmax.
