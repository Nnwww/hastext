module WordEmbedding.HasText where

import qualified Data.HashMap.Strict         as HS
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Numeric.LinearAlgebra       as LA
import qualified System.IO                   as SI
import qualified System.Random.MWC           as RM
import qualified WordEmbedding.HasText.Args  as HA
import qualified WordEmbedding.HasText.Dict  as HD
import qualified WordEmbedding.HasText.Model as HM

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State

-- The function that return a range of the dynamic window.
windowRange :: HA.Args -> HM.Model -> V.Vector T.Text -> Int -> IO (V.Vector T.Text)
windowRange args model line targetIdx = do
  winRange <- liftIO . RM.uniformR (0, negs) . HM.gRand $ model
  let winFrom = if targetIdx - winRange > 0 then targetIdx - winRange else 0
      winTo   = if V.length line > targetIdx + winRange then targetIdx + winRange else V.length line - 1
      inWindowAndNotTarget i _ = winFrom < i && i < winTo && i /= targetIdx
  return $ V.ifilter (\i e -> not $ inWindowAndNotTarget i e) line
  where
    negs = fromIntegral . HA.negatives . snd $ args

skipGram ::  Double -> V.Vector T.Text -> ReaderT HM.Params (StateT HM.Model HM.MVarIO) ()
skipGram lr line = forM_ [0..V.length line] $ \idx -> do
  args <- asks HM.args
  model <- lift get
  mapM_ (learn $ V.unsafeIndex line idx) =<< liftIO (windowRange args model line idx)
  where
    learn input target = HM.update (V.singleton input) target lr

cbow :: Double -> V.Vector T.Text -> ReaderT HM.Params (StateT HM.Model HM.MVarIO) ()
cbow lr line = forM_ [0..V.length line] $ \idx -> do
  args <- asks HM.args
  model <- lift get
  updateRange <- liftIO $ windowRange args model line idx
  HM.update updateRange (V.unsafeIndex line idx) lr

-- TODO: compare parallelization using MVar with one using ParIO.
trainThread :: HM.Params -> Integer -> HM.MVarIO HM.Params
trainThread params@HM.Params{HM.args = (method, options), HM.dict = dict, HM.tokenCountRef = tcRef} threadNo = do
  gRand <- RM.createSystemRandom
  h     <- SI.openFile (HA.input options) SI.ReadMode
  size  <- SI.hFileSize h
  SI.hSeek h SI.AbsoluteSeek $ size * threadNo `quot` threads
  let trainUntilCountUpTokens oldModel@HM.Model{HM.localTokens = lt, HM.learningRate = oldLR} = do
        tokenCountWord <- readMVar tcRef
        let tokenCount = fromIntegral tokenCountWord
        if epoch * ntokens < tokenCount
          then SI.hClose h
          else do
          let progress = tokenCount / (epoch * ntokens)
              newLR    = oldLR * (1.0 - progress) :: Double
          line <- HD.getLineFromLoopingHandle h dict gRand
          let learning = chooseMethod method newLR $ V.map HD.eword line
          newModel   <- flip execStateT oldModel $ runReaderT learning params
          newLocalTC <- bufferTokenCount $ lt + fromIntegral (V.length line)
          trainUntilCountUpTokens newModel{HM.localTokens = newLocalTC, HM.learningRate = newLR}
  trainUntilCountUpTokens $ HM.initModel dim gRand
  putStrLn $ "Finish thread " ++ show threadNo
  return params
  where
    dim     = fromIntegral $ HA.dim     options
    threads = fromIntegral $ HA.threads options
    epoch   = fromIntegral $ HA.epoch   options
    ntokens = fromIntegral $ HD.ntokens dict
    chooseMethod HA.Cbow     = cbow
    chooseMethod HA.Skipgram = skipGram
    bufferTokenCount localTokenCount
      | localTokenCount <= HA.lrUpdateTokens options = return localTokenCount
      | otherwise = do -- TODO?: logging progress rate
         modifyMVar_ tcRef (return . (+ localTokenCount))
         return 0

train :: HA.Args -> IO HM.Params
train largs@(_, opt) = do
  check
  dict  <- initDict
  wvRef <- initWVRef dict
  tcRef <- initTokenCountRef
  let params = HM.Params
        { HM.args          = largs
        , HM.dict          = dict
        , HM.sigf          = HM.genSigmoid 512 8
        , HM.logf          = HM.genLog 512
        , HM.noiseDist     = HM.genNoiseDistribution 0.75 $ HD.entries dict
        , HM.wordVecRef    = wvRef
        , HM.tokenCountRef = tcRef
        }
  x : _ <- mapConcurrently (trainThread params) [0.. fromIntegral $ HA.threads opt]
  return x
  where
    initTokenCountRef = newMVar 0
    initWVRef dic = newMVar . HS.map initW $ HD.entries dic
    initW _  = HM.Weights{HM.wI = makeVec, HM.wO = makeVec}
    makeVec  = LA.fromList $ replicate dim (0.0 :: Double)
    dim      = fromIntegral $ HA.dim opt
    initDict = HD.initFromFile largs
    check = do
      valid <- HA.checkPath largs
      unless valid $ throwString "Error: Invalid Arguments."

-- TODO: write test code using simpler corpuses, and then try to compare hastext's result with gensim's result.
--      (corpus e.g. a a a a ... b b b b ... c c c c ... d d d d ...)
-- also try to compute outsoftmax.
