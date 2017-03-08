{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module Main where

import WordEmbedding.FastText.Args

import Data.Semigroup ((<>))
import Options.Applicative.Extra (execParser)
import Options.Applicative
import Options.Applicative.Builder (command)
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


makeOptions :: Options -- default parameters
            -> Parser Options
makeOptions (Options { lr             = ra
                     , lrUpdateTokens = ut
                     , dim            = di
                     , windows        = wi
                     , epoch          = ep
                     , minCount       = mc
                     , negatives      = ne
                     , loss           = lo
                     , tSub           = ts
                     }) =
  Options <$> inputOpt
          <*> outputOpt
          <*> lrOpt
          <*> lrUpdateTokensOpt
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

    inputOpt  = mandatoryPathOpt "input"  'i' "INPUTPATH"  "training file path"
    outputOpt = mandatoryPathOpt "output" 'o' "OUTPUTPATH" "output file path"

    lrOpt             = paramOpt "lr"             'r' "RATE"      ra "learning rate"
    lrUpdateTokensOpt = paramOpt "lrUpdateTokens" 'u' "NTOKENS"   ut "number of tokens that update the learning rate"
    dimOpt            = paramOpt "dim"            'd' "DIM"       di "dimention of word vectors"
    windowsOpt        = paramOpt "windows"        'w' "WIN"       wi "size of the context window"
    epochOpt          = paramOpt "epoch"          'e' "EPOCH"     ep "number of epochs"
    minCountOpt       = paramOpt "minCount"       'c' "MINCOUNT"  mc "minimal number of word occurences"
    negativesOpt      = paramOpt "neg"            'n' "NEGATIVES" ne "number of negatives sampled"
    lossOpt           = paramOpt "loss"           'l' "LOSS"      lo "loss function {ns, hs}"
    tSubOpt           = paramOpt "tsub"           't' "TSUB"      ts "sub sampling threshold"
    threadsOpt        = paramOpt "th"             'm' "THREAD"    12 "number of threads"
    verboseOpt        = paramOpt "verbose"        'v' "LEVEL"      1 "verbosity level"

skipGram :: Mod CommandFields Args
skipGram = command "skipgram" opts
  where
    opts = info (helper <*> sgParser) (progDesc "learn representation using skipgram")
    sgParser = (Skipgram, ) <$> makeOptions skipGramDefault
    skipGramDefault = learningDefault

cbow :: Mod CommandFields Args
cbow = command "cbow" opts
  where
    opts = info (helper <*> cbowParser) (progDesc "learn representation using cbow")
    cbowParser = (Cbow, ) <$> makeOptions cbowDefault
    cbowDefault = learningDefault

parseCLI :: IO Args
parseCLI = do
  execParser parser
  where
    parser = info (helper <*> commands) (fullDesc <> header "A haskell implementation of fastText")
    commands = subparser $! skipGram <> cbow

main :: IO ()
main = do
  args <- parseCLI
  let savePath = output $ snd args
  saveArgs savePath args
  reArgs <- readArgs savePath
  return ()
