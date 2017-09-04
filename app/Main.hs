{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Data.Semigroup             ((<>))
import           Options.Applicative
import           Options.Applicative.Extra  (execParser)
import qualified WordEmbedding.HasText      as H
import           WordEmbedding.HasText.Args

{-
The following arguments are mandatory:
  --input              training file path
  --output             output file path

The following arguments are optional:
  --lr                 learning rate [0.1]
  --lrUpdateRate       change the rate of updates for the learning rate [100]
  --dim                size of word vectors [100]
  --ws                 size of the context window [5]
  --epoch              number of epochs [5]
  --minCount           minimal number of word occurences [1]
  --minCountLabel      minimal number of label occurences [0]
  --neg                number of negatives sampled [5]
  --wordNgrams         max length of word ngram [1]
  --loss               loss function {ns, hs, softmax} [ns]
  --bucket             number of buckets [2000000]
  --minn               min length of char ngram [0]
  --maxn               max length of char ngram [0]
  --thread             number of threads [12]
  --t                  sampling threshold [0.0001]
  --label              labels prefix [__label__]
  --verbose            verbosity level [2]
  --pretrainedVectors  pretrained word vectors for supervised learning []
-}


makeOptions :: HasTextArgs -- default parameters
            -> Parser HasTextArgs
makeOptions HasTextArgs {..} =
  HasTextArgs
  <$> inputOpt
  <*> outputOpt
  <*> lrOpt
  <*> lrUpdateTokensOpt
  <*> dimOpt
  <*> windowsOpt
  <*> epochOpt
  <*> minCountOpt
  <*> negativesOpt
  <*> methodOpt
  <*> lossOpt
  <*> tSubOpt
  <*> threadsOpt
  <*> verboseOpt
  where
    mandatoryPathOpt :: String -> Char -> String -> String -> Parser String
    mandatoryPathOpt longName shortName metaName helpMsg = strOption
      $! long longName
      <> short shortName
      <> metavar metaName
      <> help helpMsg

    paramOpt :: (Show a, Read a) => String -> Char -> String -> a -> String -> Parser a
    paramOpt longName shortName metaName defValue helpMsg = option auto
      $! long longName
      <> short shortName
      <> showDefault
      <> value defValue
      <> metavar metaName
      <> help helpMsg

    inputOpt  = mandatoryPathOpt "input"    'i' "INPUTPATH"  "training file path"
    outputOpt = mandatoryPathOpt "output"   'o' "OUTPUTPATH" "output file path"
    lrOpt             = paramOpt "lr"       'r' "RATE"      _initLR         "learning rate"
    lrUpdateTokensOpt = paramOpt "upd"      'u' "NTOKENS"   _lrUpdateTokens "number of tokens that update the learning rate"
    dimOpt            = paramOpt "dim"      'd' "DIM"       _dim            "dimention of word vectors"
    windowsOpt        = paramOpt "windows"  'w' "WIN"       _windows        "size of the context window"
    epochOpt          = paramOpt "epoch"    'e' "EPOCH"     _epoch          "number of epochs"
    minCountOpt       = paramOpt "minCount" 'c' "MINCOUNT"  _minCount       "minimal number of word occurences"
    negativesOpt      = paramOpt "neg"      'n' "NEGATIVES" _negatives      "number of negatives sampled"
    methodOpt         = paramOpt "method"   'm' "METHOD"    _method         "learning method {Cbow|Skipgram}"
    lossOpt           = paramOpt "loss"     'l' "LOSS"      _lossFn         "loss function {Negative|Hierarchical}"
    tSubOpt           = paramOpt "tsub"     's' "TSUB"      _tSub           "sub sampling threshold"
    threadsOpt        = paramOpt "thread"   't' "THREAD"    _threads        "number of threads"
    verboseOpt        = paramOpt "verbose"  'v' "LEVEL"     _verbose        "verbosity level"

parseCLI :: IO HasTextArgs
parseCLI = execParser parser
  where
    parser = info (makeOptions learningDefault <**> helper)
      (fullDesc <> header "hastext - Haskell Word2Vec implementation based on FastText")

main :: IO ()
main = do
  largs <- parseCLI
  w <- H.train largs
  H.saveModel w
  H.saveVectorCompat w
