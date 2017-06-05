{-# LANGUAGE ViewPatterns #-}
module WordEmbedding.HasText where

import qualified WordEmbedding.HasText.Args  as HA
import qualified WordEmbedding.HasText.Dict  as HD
import qualified WordEmbedding.HasText.Model as HM
import qualified Data.Vector                 as V
import qualified Data.Text                   as T
import qualified Data.Word                   as W
import qualified Numeric.LinearAlgebra       as LA
import qualified System.Random.MWC           as RM
import qualified System.IO                   as SI

import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

-- The function that return a range of the dynamic window.
windowRange :: V.Vector T.Text -> HM.Model -> Int -> ReaderT HM.Params IO (V.Vector T.Text)
windowRange line model targetIdx = do
  negs     <- asks $ fromIntegral . HA.negatives . snd . HM.args
  winRange <- liftIO . RM.uniformR (0, negs) . HM.gRand $ model
  let winFrom = if targetIdx - winRange > 0 then targetIdx - winRange else 0
      winTo   = if V.length line > targetIdx + winRange then targetIdx + winRange else V.length line - 1
      inWindowAndNotTarget i _ = winFrom < i && i < winTo && i /= targetIdx
  return $ V.ifilter (\i e -> not $ inWindowAndNotTarget i e) line

skipGram :: Double -> V.Vector T.Text -> ReaderT HM.Params (StateT HM.Model HM.MVarIO) ()
skipGram lr line = forM_ [0..V.length line] $ \idx -> do
  model <- lift get
  params <- ask
  updateRange <- liftIO $ windowRange line model idx `runReaderT` params
  mapM_ (learn $ V.unsafeIndex line idx) updateRange
  where
    learn input target = HM.update (V.singleton input) target lr

cbow :: Double -> V.Vector T.Text -> ReaderT HM.Params (StateT HM.Model HM.MVarIO) ()
cbow lr line = forM_ [0..V.length line] $ \idx -> do
  model <- lift get
  params <- ask
  updateRange <- liftIO $ windowRange line model idx `runReaderT` params
  HM.update updateRange (V.unsafeIndex line idx) lr


-- TODO: compare parallelization using MVar with one using ParIO.
trainThread :: MVar W.Word -> Integer -> ReaderT HM.Params (StateT HM.Model HM.MVarIO) ()
trainThread tokenCountRef threadNo = do
  (method, options) <- asks HM.args
  dict  <- asks HM.dict
  h     <- liftIO $ SI.openFile (HA.input options) SI.ReadMode
  size  <- liftIO $ SI.hFileSize h
  let threads = fromIntegral . HA.threads $ options
  liftIO . SI.hSeek h SI.AbsoluteSeek $ size * threadNo `quot` threads
  let trainUntilCountUpTokens localTokenCount oldLR = do
        tokenCountWord <- liftIO $ readMVar tokenCountRef
        let epoch      = fromIntegral $ HA.epoch options
            ntokens    = fromIntegral $ HD.ntokens dict
            tokenCount = fromIntegral tokenCountWord
        if epoch * ntokens < tokenCount
          then liftIO $ SI.hClose h
          else do
          let progress = tokenCount / epoch * ntokens
              lr = oldLR * (1.0 - progress) :: Double
          gRand <- lift   $ gets HM.gRand
          line  <- liftIO $ HD.getLineFromLoopingHandle h dict gRand
          (chooseMethod method) lr $ V.map HD.eword line
          newLocalTC <- liftIO $ bufferTokenCount options (localTokenCount + (fromIntegral $ V.length line))
          trainUntilCountUpTokens newLocalTC lr
  trainUntilCountUpTokens 0 $ HA.lr options
  liftIO . putStrLn $ "Finish thread " ++ show threadNo
  where
    chooseMethod HA.Cbow     = cbow
    chooseMethod HA.Skipgram = skipGram
    bufferTokenCount options localTokenCount
      | localTokenCount <= HA.lrUpdateTokens options = return localTokenCount
      | otherwise = do -- TODO?: logging progress rate
         modifyMVar_ tokenCountRef (return . (+ localTokenCount))
         return 0

train :: HA.Args -> IO ()
train args = do
  dict <- initDict

  return ()
  where
    initDict = do
      available <- HA.checkPath args
      HD.initFromFile $ assert available args





-- TODO: write test code using simpler corpuses, and then try to compare hastext's result with gensim's result.
--      (corpus e.g. a a a a ... b b b b ... c c c c ... d d d d ...)
-- also try to compute outsoftmax.
