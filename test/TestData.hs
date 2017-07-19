{-# LANGUAGE OverloadedStrings #-}
module TestData where

import           Paths_hastext
import           Data.Monoid
import           Data.List                  as L
import           Data.Text                  as T
import           WordEmbedding.HasText.Args

noFailInputList :: [Text]
noFailInputList = mconcat . L.map (L.replicate 5 . T.singleton) $ ['a' .. 'e']

noFailPath, text8Path :: IO FilePath
noFailPath = getDataFileName "data/NonFail.txt"
text8Path  = getDataFileName "data/text8s/text8_1k"

noFailParams, noFailOnMultiThreadParams, text8RunParams :: IO HasTextArgs
noFailParams = do
  inputFilePath <- noFailPath
  return (Skipgram, noFailDefault{ _input  = inputFilePath
                                 , _output = inputFilePath <> ".out"})
noFailOnMultiThreadParams = do
  inputFilePath <- noFailPath
  return (Skipgram, noFailDefault{ _input   = inputFilePath
                                 , _output  = inputFilePath <> ".out"
                                 , _threads = 4})
text8RunParams = do
  inputFilePath <- text8Path
  return (Skipgram, text8RunDefault{ _input  = inputFilePath
                                   , _output = inputFilePath <> ".out"})

noFailDefault,text8RunDefault :: HasTextOptions
noFailDefault =  HasTextOptions
  { _input          = ""
  , _output         = ""
  , _initLR         = 0.05
  , _lrUpdateTokens = 100
  , _dim            = 100
  , _windows        = 2
  , _epoch          = 5
  , _minCount       = 1
  , _negatives      = 5
  , _lossFn         = Negative
  , _tSub           = 0.0001
  , _threads        = 1
  , _verbose        = 1
  }

text8RunDefault = noFailDefault
  { _input    = ""
  , _output   = ""
  , _dim      = 100
  , _windows  = 5
  , _minCount = 5
  , _threads  = 8
  }
