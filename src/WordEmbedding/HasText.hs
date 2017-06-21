{-# LANGUAGE ScopedTypeVariables #-}
module WordEmbedding.HasText where

import           Data.Ord
import           Data.Bifunctor
import qualified Data.HashMap.Strict              as HS
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import qualified Data.Vector.Algorithms.Intro     as VA
import qualified Numeric.LinearAlgebra            as LA
import qualified System.IO                        as SI
import qualified System.Random.MWC                as RM
import qualified System.Random.MWC.CondensedTable as RMC
import qualified WordEmbedding.HasText.Args       as HA
import qualified WordEmbedding.HasText.Dict       as HD
import qualified WordEmbedding.HasText.Model      as HM

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.ST
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

skipGram :: Double -> V.Vector T.Text -> ReaderT HM.Params (StateT HM.Model HM.MVarIO) ()
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

-- TODO: compare parallelization using MVar with one using ParIO etc.
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
          line <- HD.getLineLoop h dict gRand
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

-- | The result of Word2Vec method.
-- In contrast to Model type, it is more preferable that each label of this record is lazy evoluted.
data Word2Vec = Word2Vec
  { _args          :: HA.Args
  , _dict          :: HD.Dict
  , _sigf          :: Double -> Double             -- ^ (memorized) sigmoid function
  , _logf          :: Double -> Double             -- ^ (memorized) log function
  , _noiseDist     :: RMC.CondensedTableV HD.Entry -- ^ noise distribution table
  , _wordVec       :: HM.WordVec                   -- ^ word vectors
  }

train :: HA.Args -> IO Word2Vec
train args@(_, opt) = do
  check
  dict <- initDict
  wvRef <- initWVRef dict
  tcRef <- initTokenCountRef
  let params = HM.Params
        { HM.args          = args
        , HM.dict          = dict
        , HM.sigf          = HM.genSigmoid 512 8
        , HM.logf          = HM.genLog 512
        , HM.noiseDist     = HM.genNoiseDistribution 0.75 $ HD.entries dict
        , HM.wordVecRef    = wvRef
        , HM.tokenCountRef = tcRef
        }
  resultParams : _ <- mapConcurrently (trainThread params) [0.. fromIntegral $ HA.threads opt]
  resultWordVec <- readMVar $ HM.wordVecRef resultParams
  return Word2Vec
    { _args      = HM.args resultParams
    , _dict      = HM.dict resultParams
    , _sigf      = HM.sigf resultParams
    , _logf      = HM.logf resultParams
    , _noiseDist = HM.noiseDist resultParams
    , _wordVec   = resultWordVec
    }
  where
    initTokenCountRef = newMVar 0
    initWVRef = newMVar . HS.map initW . HD.entries
    initW _   = HM.Weights{HM.wI = makeVec, HM.wO = makeVec}
    makeVec   = LA.fromList $ replicate dim (0.0 :: Double)
    dim       = fromIntegral $ HA.dim opt
    initDict  = HD.initFromFile args
    check = do
      validOpts <- HA.validOpts args
      unless validOpts $ throwString "Error: Invalid Arguments."

data ErrMostSim = EmptyInput
                | AbsenceOfWords {absPosW :: [T.Text], negPosW :: [T.Text]}
                -- ^ words that do not exist in trained corpora when execute mostSimilar.

-- TODO: I would typing input lists using liquidhaskell.
-- | Get a most similar word list. Note that the result list is a delayed version of the entire dictionary.
mostSimilar :: Word2Vec
            -> [T.Text] -- ^ positive words
            -> [T.Text] -- ^ negative words
            -> Either ErrMostSim [(T.Text, Double)]
mostSimilar Word2Vec{_wordVec = wv} positives negatives
  | length absPoss /= 0 || length absNegs /= 0 = Left $ AbsenceOfWords absPoss absNegs
  | otherwise = Right . V.toList $ cosSims
  where
    absPoss = absentWords positives
    absNegs = absentWords negatives
    absentWords = filter (not . flip HS.member wv)
    cosSims = runST $ do
      cosSimVecs <- V.unsafeThaw . V.map (second $ cosSim . HM.wI) . V.fromList $ HS.toList wv
      VA.sortBy (flip $ comparing snd) cosSimVecs
      V.unsafeFreeze cosSimVecs
    cosSim x = LA.dot unitedMean (unitVector x)
    unitedMean = unitVector mean
    unitVector (v :: LA.Vector Double) = (1 / LA.norm_2 v) `LA.scale` v
    mean = LA.scale (1 / inputLength) . foldr1 LA.add $ (map getAndPosScale positives) ++ (map getAndNegScale negatives)
    inputLength = fromIntegral $ (length positives) + (length negatives)
    getAndPosScale = getVec
    getAndNegScale = LA.scale (-1) . getVec
    getVec = HM.wI . (wv HS.!)


-- TODO: write test code using simpler corpuses, and then try to compare hastext's result with gensim's result.
--      (corpus e.g. a a a a ... b b b b ... c c c c ... d d d d ...)
-- also try to compute outsoftmax.
