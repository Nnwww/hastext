module Main where

import Lib

import Data.Semigroup ((<>))
import Data.Char
import Text.Read
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
  , threads   :: Int
  , dim       :: Int
  , minCount  :: Int
  , epoch     :: Int
  , loss      :: Loss
  , windows   :: Int
  , tSub      :: Double
  , negatives :: Int
  , verbose   :: Bool
  } deriving Show

data Loss = Negative | Hierarchical deriving Show


main :: IO ()
main = someFunc
