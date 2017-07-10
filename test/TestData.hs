{-# LANGUAGE OverloadedStrings #-}
module TestData where

import           Paths_hastext
import           Data.Monoid
import           Data.List                  as L
import           Data.Text                  as T
import qualified WordEmbedding.HasText.Args as HA

noFailInputList :: [Text]
noFailInputList = mconcat . L.map (L.replicate 5 . T.singleton) $ ['a' .. 'e']

noFailPath, text8Path :: IO FilePath
noFailPath = getDataFileName "data/NonFail.txt"
text8Path  = getDataFileName "data/text8s/text8_1k"

noFailParams, noFailOnMultiThreadParams, text8RunParams :: IO HA.Args
noFailParams = do
  inputFilePath <- noFailPath
  return (HA.Skipgram, noFailDefault{ HA.input  = inputFilePath
                                    , HA.output = inputFilePath <> ".out"})
noFailOnMultiThreadParams = do
  inputFilePath <- noFailPath
  return (HA.Skipgram, noFailDefault{ HA.input   = inputFilePath
                                    , HA.output  = inputFilePath <> ".out"
                                    , HA.threads = 4})
text8RunParams = do
  inputFilePath <- text8Path
  return (HA.Skipgram, text8RunDefault{ HA.input  = inputFilePath
                                      , HA.output = inputFilePath <> ".out"})

noFailDefault,text8RunDefault :: HA.Options
noFailDefault =  HA.Options
  { HA.input          = ""
  , HA.output         = ""
  , HA.lr             = 0.05
  , HA.lrUpdateTokens = 100
  , HA.dim            = 100
  , HA.windows        = 2
  , HA.epoch          = 5
  , HA.minCount       = 1
  , HA.negatives      = 5
  , HA.loss           = HA.Negative
  , HA.tSub           = 0.0001
  , HA.threads        = 1
  , HA.verbose        = 1
  }
text8RunDefault = noFailDefault
  { HA.input    = ""
  , HA.output   = ""
  , HA.dim      = 100
  , HA.windows  = 5
  , HA.minCount = 5
  , HA.threads  = 8
  }
