{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module WordEmbedding.HasText where

import           Data.Ord
import           Data.Bifunctor
import           Data.Semigroup
import qualified Data.Binary                      as B
import qualified Data.HashMap.Strict              as HS
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TI
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
import           TextShow

-- | The result of Word2Vec method.
-- In contrast to LParams type, it is more preferable that each label of this record is lazy evoluted.
data Word2Vec = Word2Vec
  { _args          :: HA.Args
  , _dict          :: HD.Dict
  , _sigf          :: Double -> Double             -- ^ (memorized) sigmoid function
  , _logf          :: Double -> Double             -- ^ (memorized) log function
  , _noiseDist     :: RMC.CondensedTableV HD.Entry -- ^ noise distribution table
  , _wordVec       :: HM.WordVec                   -- ^ word vectors
  }

instance B.Binary Word2Vec where
  get = do
    a <- B.get
    d <- B.get
    w <- B.get
    return Word2Vec
      { _args      = a
      , _dict      = d
      , _sigf      = HM.genSigmoid 512 8
      , _logf      = HM.genLog 512
      , _noiseDist = HM.genNoiseDistribution 0.75 $ HD.entries d
      , _wordVec   = w
      }
  put Word2Vec{_args = a, _dict = d, _wordVec = w} = B.put a >> B.put d >> B.put w

-- The function that return a range of the dynamic window.
unsafeWindowRange :: HA.Args -> HM.LParams -> V.Vector T.Text
                  -> Int -- ^ The central index of a window. Note that no boundary checks.
                  -> IO (V.Vector T.Text)
unsafeWindowRange args model line targetIdx = do
  winRange <- RM.uniformR (0, negs) . HM.gRand $ model
  let winFrom = if targetIdx - winRange > 0 then targetIdx - winRange else 0
      winTo   = if V.length line > targetIdx + winRange then targetIdx + winRange else V.length line - 1
      inWindowAndNotTarget i _ = winFrom < i && i < winTo && i /= targetIdx
  return $ V.ifilter (\i e -> not $ inWindowAndNotTarget i e) line
  where
    negs = fromIntegral . HA.negatives . snd $ args

skipGram :: V.Vector T.Text -> ReaderT HM.Params (StateT HM.LParams HM.MVarIO) ()
skipGram line = forM_ [0..V.length line - 1] $ \idx -> do
  args <- asks HM.args
  model <- lift get
  mapM_ (learn $ V.unsafeIndex line idx) =<< liftIO (unsafeWindowRange args model line idx)
  where
    learn input target = HM.update (V.singleton input) target

cbow :: V.Vector T.Text -> ReaderT HM.Params (StateT HM.LParams HM.MVarIO) ()
cbow line = forM_ [0..V.length line - 1] $ \idx -> do
  args <- asks HM.args
  model <- lift get
  updateRange <- liftIO $ unsafeWindowRange args model line idx
  HM.update updateRange (V.unsafeIndex line idx)

-- TODO: compare parallelization using MVar with one using ParIO etc.
trainThread :: HM.Params -> Integer -> HM.MVarIO HM.Params
trainThread params@HM.Params{HM.args = (lm, opt), HM.dict = dict, HM.tokenCountRef = tcRef} threadNo = do
  gRand <- RM.createSystemRandom
  h     <- SI.openFile (HA.input opt) SI.ReadMode
  size  <- SI.hFileSize h
  SI.hSeek h SI.AbsoluteSeek $ size * threadNo `quot` (fromIntegral $ HA.threads opt)
  let trainUntilCountUpTokens localTC oldLR oldLParams = do
        tokenCount <- readMVar tcRef
        if tokens < tokenCount
          then SI.hClose h
          else do
          let progress :: Double = fromIntegral tokenCount / fromIntegral tokens
              newLR    = oldLR * (1.0 - progress)
          line <- HD.getLineLoop h dict gRand
          let learning = method $ V.map HD.eword line
          newLParams   <- flip execStateT oldLParams $ runReaderT learning params{HM.lr = newLR}
          newLocalTC <- bufferTokenCount $ localTC + fromIntegral (V.length line)
          trainUntilCountUpTokens newLocalTC newLR newLParams
  trainUntilCountUpTokens 0 (HA.lr opt) $ HM.initLParams (fromIntegral $ HA.dim opt) gRand
  putStrLn $ "Finish thread " ++ show threadNo
  return params
  where
    tokens = (HA.epoch opt) * (HD.ntokens dict)
    method  = chooseMethod lm
    chooseMethod HA.Cbow     = cbow
    chooseMethod HA.Skipgram = skipGram
    bufferTokenCount localTokenCount
      | localTokenCount <= HA.lrUpdateTokens opt = return localTokenCount
      | otherwise = do -- TODO?: logging progress rate
         modifyMVar_ tcRef (return . (+ localTokenCount))
         return 0

train :: HA.Args -> IO Word2Vec
train args@(_, opt) = do
  check
  dict  <- initDict
  rand  <- RM.createSystemRandom
  wvRef <- initWVRef rand dict
  tcRef <- initTokenCountRef
  let params = HM.Params
        { HM.args          = args
        , HM.dict          = dict
        , HM.lr            = HA.lr opt
        , HM.sigf          = HM.genSigmoid 512 8
        , HM.logf          = HM.genLog 512
        , HM.noiseDist     = HM.genNoiseDistribution 0.75 $ HD.entries dict
        , HM.wordVecRef    = wvRef
        , HM.tokenCountRef = tcRef
        }
  resultParams : _ <- mapConcurrently (trainThread params) [0.. fromIntegral $ HA.threads opt - 1]
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
    initDict = HD.initFromFile args
    check = do
      validOpts <- HA.validOpts args
      unless validOpts $ throwString "Error: Invalid Arguments."
    initWVRef :: RM.GenIO -> HD.Dict -> IO HM.WordVecRef
    initWVRef r d = (newMVar . HS.fromList) =<< (sequence . map (initW r) . HS.keys $ HD.entries d)
      where
        dim :: forall a . Num a => a
        dim = fromIntegral $ HA.dim opt
        zeros = LA.fromList $ replicate dim (0.0 :: Double)
        initW gr k = do
          randoms <- forM [1 .. dim :: Int] $ \_ -> RM.uniformR (-1 / dim, 1 / dim) gr
          return (k, HM.Weights{HM.wI = LA.fromList randoms, HM.wO = zeros})


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
  | otherwise = Right . V.toList $ sortedCosSims
  where
    absPoss = absentWords positives
    absNegs = absentWords negatives
    absentWords = filter (not . flip HS.member wv)
    sortedCosSims = runST $ do
      cosSimVecs <- V.unsafeThaw . V.map (second $ cosSim . HM.wI) . V.fromList $ HS.toList wv
      VA.sortBy (flip $ comparing snd) cosSimVecs
      V.unsafeFreeze cosSimVecs
    cosSim x = LA.dot (unitVector mean) (unitVector x)
    unitVector (v :: LA.Vector Double) = LA.scale (1 / LA.norm_2 v) v
    mean = LA.scale (1 / inputLength) . foldr1 LA.add $ (map getAndPosScale positives <> map getAndNegScale negatives)
    inputLength = fromIntegral $ (length positives) + (length negatives)
    getAndPosScale = getVec
    getAndNegScale = LA.scale (-1) . getVec
    getVec = HM.wI . (wv HS.!)

saveModel :: (MonadIO m, MonadThrow m) => Word2Vec -> m ()
saveModel w@Word2Vec{_args = args} = liftIO $ B.encodeFile (HA.output . snd $ args) w

saveVectorCompat :: (MonadIO m, MonadThrow m) => Word2Vec -> m ()
saveVectorCompat Word2Vec{_args = args, _dict = dict, _wordVec = wv} =
  liftIO . SI.withFile (outFilePath <> ".vecc") SI.WriteMode $ \h -> do
    TI.hPutStrLn h $ toText sizeAndDim
    mapM_ (putVec h) $ HS.toList wv
  where
    outFilePath = HA.output . snd $ args
    sizeAndDim = (showb . HS.size $ HD.entries dict) <> showbSpace <> (showb . HA.dim $ snd args)
    putVec h (k, HM.Weights{HM.wI = i}) =
      TI.hPutStrLn h . toText $ (fromText k) <> showbSpace <> (unwordsB . map showb $ LA.toList i)

loadModel :: (MonadIO m, MonadThrow m) => FilePath -> m Word2Vec
loadModel fpath = liftIO $ B.decodeFile fpath

loadVectorCompat :: (MonadIO m, MonadThrow m) => FilePath -> m Word2Vec
loadVectorCompat fpath = undefined
