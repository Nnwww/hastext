{-# LANGUAGE OverloadedStrings #-}
module TestData where

import           Paths_hastext
import           Data.List                  as L
import           Data.Text                  as T
import qualified WordEmbedding.HasText.Args as HA

noFailInputList :: [Text]
noFailInputList = mconcat . L.map (L.replicate 5 . T.singleton) $ ['a' .. 'e']

noFailPath = getDataFileName "data/NonFail.txt"

noFailParams = do
  inputFilePath <- noFailPath
  return (HA.Skipgram, learningDefault{ HA.input  = inputFilePath
                                      , HA.output = inputFilePath ++ ".out"
                                      })

learningDefault :: HA.Options
learningDefault = HA.Options
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
