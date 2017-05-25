module WordEmbedding.HasText where

import qualified WordEmbedding.HasText.Args  as HA
import qualified WordEmbedding.HasText.Dict  as HD
import qualified WordEmbedding.HasText.Model as HM
import qualified Data.Vector                  as V
import qualified Data.Text                    as T
import qualified Data.Word                    as W
import qualified Numeric.LinearAlgebra        as LA
import qualified System.Random.MWC            as RM
import qualified System.IO                    as SI

import Control.Concurrent
import Control.Monad.ST
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

-- The function that return a range of the dynamic window.
windowRange :: V.Vector T.Text -> HM.Model -> Int -> ReaderT HM.Params IO (V.Vector T.Text)
windowRange line model targetIdx = do
  negs <- asks $ fromIntegral . HA.negatives . snd . HM.args
  winRange <- liftIO . RM.uniformR (0, negs) . HM.gRand $ model
  let winFrom = if targetIdx - winRange > 0 then targetIdx - winRange else 0
      winTo   = if V.length line > targetIdx + winRange then targetIdx + winRange else V.length line - 1
      inWindowAndNotTarget i _ = winFrom < i && i < winTo && i /= targetIdx
  return $ V.ifilter inWindowAndNotTarget line

skipGram ::  Double -> V.Vector T.Text -> ReaderT HM.Params (StateT HM.Model HM.MVarIO) ()
skipGram lr line = updateEachWinElems . V.zip (V.fromList [0..]) $ line
  where
    updateEachWinElems idxAndWords = do
      let h = V.head
      updateRange <- windowRange line m targetIdx
      V.foldM (learn updTarget) m updateRange
    learn target acc e = do
      acc
      HM.update (V.singleton target) e lr



cbow :: HM.Model -> Double -> V.Vector T.Text -> IO HM.Model
cbow model lr line = V.ifoldM update model line
  where
    update m targetIdx updTarget = do
      inputRange <- windowRange line m targetIdx
      HM.update m inputRange updTarget lr

-- TODO: compare parallelization using MVar with one using ParIO.
trainThread :: HM.Model -> MVar W.Word -> Integer -> IO ()
trainThread model tokenCountRef threadNo = do
  h <- SI.openFile inputPath SI.ReadMode
  size <- SI.hFileSize h
  SI.hSeek h SI.AbsoluteSeek (size * threadNo `quot` threads)

  SI.hClose h
  where
    inputPath = HA.input . snd $ args
    threads = fromIntegral . HA.threads . snd $ args
    ntokens = HD.ntokens . HM.dict $ model
    epoch = undefined
    trainLoop  = undefined


-- TODO: write test code using simpler corpuses, and then try to compare hastext's result with gensim's result.
--      (corpus e.g. a a a a ... b b b b ... c c c c ... d d d d ...)
-- also try to compute outsoftmax.
