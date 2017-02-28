module Main where

import Lib

import Data.Semigroup ((<>))
import Options.Applicative

{-
The following arguments are mandatory:
  -input              training file path
  -output             output file path

The following arguments are optional:
  -lr                 learning rate [0.1]
  -lrUpdateRate       change the rate of updates for the learning rate [100]
  -dim                size of word vectors [100]
  -ws                 size of the context window [5]
  -epoch              number of epochs [5]
  -minCount           minimal number of word occurences [1]
  -minCountLabel      minimal number of label occurences [0]
  -neg                number of negatives sampled [5]
  -wordNgrams         max length of word ngram [1]
  -loss               loss function {ns, hs, softmax} [ns]
  -bucket             number of buckets [2000000]
  -minn               min length of char ngram [0]
  -maxn               max length of char ngram [0]
  -thread             number of threads [12]
  -t                  sampling threshold [0.0001]
  -label              labels prefix [__label__]
  -verbose            verbosity level [2]
  -pretrainedVectors  pretrained word vectors for supervised learning []
-}

data Options = Options
  { input     :: FilePath
  , output    :: FilePath
  , lRate     :: Double
  , dim       :: Int
  , windows   :: Int
  , epoch     :: Int
  , minCount  :: Int
  , negatives :: Int
  , loss      :: Loss
  , tSub      :: Double
  , threads   :: Int
  , verbose   :: Int
  } deriving (Show)

data Loss = Negative | Hierarchical deriving (Show, Read)

data DefalutOpt = DefaultOpt
  { dLRate     :: Double
  , dDim       :: Int
  , dWindows   :: Int
  , dEpoch     :: Int
  , dMinCount  :: Int
  , dNegatives :: Int
  , dLoss      :: Loss
  , dTSub      :: Double
  }

makeOptions :: DefalutOpt -- default parameters
            -> Parser Options
makeOptions (DefaultOpt { dLRate     = lr
                        , dDim       = di
                        , dWindows   = wi
                        , dEpoch     = ep
                        , dMinCount  = mc
                        , dNegatives = ne
                        , dLoss      = lo
                        , dTSub      = ts
                        }) =
  Options <$> inputOpt
          <*> outputOpt
          <*> lRateOpt
          <*> dimOpt
          <*> windowsOpt
          <*> epochOpt
          <*> minCountOpt
          <*> negativesOpt
          <*> lossOpt
          <*> tSubOpt
          <*> threadsOpt
          <*> verboseOpt
  where
    inputOpt = strOption
      $  long "input"
      <> short 'i'
      <> metavar "INPUTPATH"
      <> help "training file path"

    outputOpt = strOption
      $  long "output"
      <> short 'o'
      <> metavar "OUTPUTPATH"
      <> help "output file path"

    paramOpt :: (Show a, Read a) => String -> Char -> String -> a -> String -> Parser a
    paramOpt longName shortName metaName defValue helpMsg = option auto
      $  long longName
      <> short shortName
      <> showDefault
      <> value defValue
      <> metavar metaName
      <> help helpMsg

    lRateOpt     = paramOpt "lRate"    'r' "RATE"      lr "learning rate"
    dimOpt       = paramOpt "dim"      'd' "DIM"       di "dimention of word vectors"
    windowsOpt   = paramOpt "windows"  'w' "WIN"       wi "size of the context window"
    epochOpt     = paramOpt "epoch"    'e' "EPOCH"     ep "number of epochs"
    minCountOpt  = paramOpt "minCount" 'c' "MINCOUNT"  mc "minimal number of word occurences"
    negativesOpt = paramOpt "neg"      'n' "NEGATIVES" ne "number of negatives sampled"
    lossOpt      = paramOpt "loss"     'l' "LOSS"      lo "loss function {ns, hs}"
    tSubOpt      = paramOpt "tsub"     't' "TSUB"      ts "sub sampling threshold"
    threadsOpt   = paramOpt "th"       'm' "THREAD"    12 "number of threads"
    verboseOpt   = paramOpt "verbose"  'v' "LEVEL"      1 "verbosity level"

main :: IO ()
main = someFunc
