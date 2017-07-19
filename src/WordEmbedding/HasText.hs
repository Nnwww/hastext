{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections #-}

module WordEmbedding.HasText
  ( HasTextResult(..)
  , train
  , loadModel
  , loadVectorCompat
  , saveModel
  , saveVectorCompat
  , ErrMostSim(..)
  , mostSimilar
  , mostSimilarN
  ) where

import           Data.Ord
import           Data.Bifunctor
import           Data.Semigroup
import           Data.Mutable
import qualified Data.Binary                         as B
import qualified Data.HashMap.Strict                 as HS
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as TI
import qualified Data.Vector                         as V
import qualified Data.Vector.Unboxed                 as VU
import qualified Data.Vector.Algorithms.Intro        as VA
import qualified System.IO                           as SI
import qualified System.Random.MWC                   as RM
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Reader
import           TextShow
import           WordEmbedding.HasText.Args
import           WordEmbedding.HasText.Dict
import           WordEmbedding.HasText.Model
import           WordEmbedding.HasText.Internal.Strict.HasText
import           WordEmbedding.HasText.Internal.Type
                 (HasTextResult(..))

instance B.Binary HasTextResult where
  get = do
    a <- B.get
    d <- B.get
    w <- B.get
    return HasTextResult
      { htArgs      = a
      , htDict      = d
      , htSigf      = genSigmoid 512 8
      , htLogf      = genLog 512
      , htNoiseDist = genNoiseDistribution 0.75 $ _entries d
      , htWordVec   = w
      }
  put HasTextResult{htArgs = a, htDict = d, htWordVec = w} = B.put a >> B.put d >> B.put w


skipGram :: V.Vector T.Text -> Model
skipGram line = forM_ [0..V.length line - 1] $ \idx -> do
  (Params{_args = a}, lp) <- ask
  mapM_ (learn $ V.unsafeIndex line idx) =<< liftIO (unsafeWindowRange a lp line idx)
  where
    learn input target = updateModel (V.singleton input) target

cbow :: V.Vector T.Text -> Model
cbow line = forM_ [0..V.length line - 1] $ \idx -> do
  (a, lp) <- asks $ first _args
  updateRange <- liftIO $ unsafeWindowRange a lp line idx
  updateModel updateRange (V.unsafeIndex line idx)

-- TODO: compare parallelization using MVar with one using ParIO etc.
trainThread :: Params -> Integer -> IO Params
trainThread params@Params{_args = (lm, opt), _dict = dict, _tokenCountRef = tcRef} threadNo = do
  gRand <- RM.createSystemRandom
  h     <- SI.openFile (_input opt) SI.ReadMode
  size  <- SI.hFileSize h
  SI.hSeek h SI.AbsoluteSeek $ size * threadNo `quot` (fromIntegral $ _threads opt)
  let trainUntilCountUpTokens !localTC oldLR lParams = do
        tokenCount <- atomicModifyRef' tcRef (\tc -> (tc,fromIntegral tc))
        if tokens < tokenCount
          then SI.hClose h
          else do
          let (progress :: Double) = fromIntegral tokenCount / fromIntegral tokens
              newLR                = oldLR * (1.0 - progress)
          line <- getLineLoop h dict gRand
          let learning = method $ V.map _eWord line
          runReaderT learning (params{_lr = newLR}, lParams)
          newLocalTC <- bufferTokenCount $ localTC + fromIntegral (V.length line)
          trainUntilCountUpTokens newLocalTC newLR lParams
  trainUntilCountUpTokens 0 (_initLR opt) =<< (initLParams (fromIntegral $ _dim opt) $ gRand)
  putStrLn $ "Finish thread " ++ show threadNo
  return params
  where
    tokens = (_epoch opt) * (_ntokens dict)
    method = chooseMethod lm
    chooseMethod Cbow     = cbow
    chooseMethod Skipgram = skipGram
    bufferTokenCount :: Word -> IO Word
    bufferTokenCount localTokenCount
      | localTokenCount <= _lrUpdateTokens opt = return localTokenCount
      | otherwise = do -- TODO?: logging progress rate
         atomicModifyRef' tcRef ((,()) . (+ localTokenCount))
         return 0

train :: HasTextArgs -> IO HasTextResult
train args@(_, opt) = do
  check
  dict  <- initDict
  rand  <- RM.createSystemRandom
  wvRef <- initWVRef rand dict
  tcRef <- newRef 0
  let params = Params
        { _args          = args
        , _dict          = dict
        , _lr            = _initLR opt
        , _sig           = genSigmoid 512 8
        , _log           = genLog 512
        , _noiseDist     = genNoiseDistribution 0.75 $ _entries dict
        , _wordVecRef    = wvRef
        , _tokenCountRef = tcRef
        }
  resultParams : _ <- mapConcurrently (trainThread params) [0.. fromIntegral $ _threads opt - 1]
  immWordVec <- unsafeFreezeWordVecRef $ _wordVecRef resultParams
  return HasTextResult
    { htArgs      = _args      resultParams
    , htDict      = _dict      resultParams
    , htSigf      = _sig       resultParams
    , htLogf      = _log       resultParams
    , htNoiseDist = _noiseDist resultParams
    , htWordVec   = immWordVec
    }
  where
    initDict = initFromFile args
    unsafeFreezeWordVecRef wvRef = fmap HS.fromList . mapM (\(k,v) -> (k,) <$> unsafeFreezeMW v) . HS.toList =<< readMVar wvRef
    check = validOpts args >>= (flip unless) (throwString "Error: Invalid Arguments.")
    initWVRef :: RM.GenIO -> Dict -> IO WordVecRef
    initWVRef rnd d = (newMVar . HS.fromList) =<< (sequence . map initWVEntries . HS.keys $ _entries d)
      where
        initWVEntries k = (k,) <$> initMW rnd (fromIntegral $ _dim opt)

data ErrMostSim = EmptyInput
                | AbsenceOfWords {absPosW :: [T.Text], negPosW :: [T.Text]}
                -- ^ words that do not exist in trained corpora when execute mostSimilar.

-- TODO: I would like to type input lists using liquidhaskell.
-- | Get a most similar word list. Note that the result list is a delayed version of the entire dictionary.
mostSimilar :: HasTextResult
            -> Word     -- ^ from
            -> Word     -- ^ to
            -> [T.Text] -- ^ positive words
            -> [T.Text] -- ^ negative words
            -> Either ErrMostSim [(T.Text, Double)]
mostSimilar HasTextResult{htWordVec = wv} from to positives negatives
  | length positives == 0 && length negatives == 0 = Left EmptyInput
  | length absPoss   /= 0 || length absNegs   /= 0 = Left $ AbsenceOfWords absPoss absNegs
  | otherwise = Right . slice from to $ V.toList sortedCosSims
  where
    sortedCosSims = runST $ do
      cosSimVecs <- V.unsafeThaw . V.map (second $ cosSim mean . _wI) . V.fromList $ HS.toList wv
      VA.sortBy (flip $ comparing snd) cosSimVecs
      V.unsafeFreeze cosSimVecs
    mean         = scale (1 / inputLength) $ foldr1 addUU scaledInputs
    scaledInputs = map getPosScale positives <> map getNegScale negatives
    absPoss      = absentWords positives
    absNegs      = absentWords negatives
    absentWords  = filter (not . flip HS.member wv)
    inputLength  = fromIntegral $ (length positives) + (length negatives)
    getPosScale  = getVec
    getNegScale  = scale (-1) . getVec
    getVec       = _wI . (wv HS.!)
    slice f t xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)
      where
        start = fromIntegral f
        stop  = fromIntegral t

-- | Such synonym of mostSimilar as it return 0-top n.
mostSimilarN :: HasTextResult
            -> Word     -- ^ top n
            -> [T.Text] -- ^ positive words
            -> [T.Text] -- ^ negative words
            -> Either ErrMostSim [(T.Text, Double)]
mostSimilarN w topn positives negatives = mostSimilar w 0 topn positives negatives

saveModel :: (MonadIO m, MonadThrow m) => HasTextResult -> m ()
saveModel w@HasTextResult{htArgs = args} = liftIO $ B.encodeFile outFilePath w
  where
    outFilePath = _output . snd $ args

saveVectorCompat :: (MonadIO m, MonadThrow m) => HasTextResult -> m ()
saveVectorCompat HasTextResult{htArgs = args, htDict = dict, htWordVec = wv} =
  liftIO . SI.withFile (outFilePath <> ".vecc") SI.WriteMode $ \h -> do
    TI.hPutStrLn h $ toText sizeAndDim
    mapM_ (putVec h) $ HS.toList wv
  where
    outFilePath = _output . snd $ args
    sizeAndDim = (showb . HS.size $ _entries dict) <> showbSpace <> (showb . _dim $ snd args)
    putVec h (k, Weights{_wI = i}) =
      TI.hPutStrLn h . toText $ (fromText k) <> showbSpace <> (unwordsB . map showb $ VU.toList i)

loadModel :: (MonadIO m, MonadThrow m) => FilePath -> m HasTextResult
loadModel fpath = liftIO $ B.decodeFile fpath

loadVectorCompat :: (MonadIO m, MonadThrow m) => FilePath -> m HasTextResult
loadVectorCompat fpath = undefined
