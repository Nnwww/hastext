{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup
import Criterion.Main
import Paths_hastext
import WordEmbedding.HasText.Args
import WordEmbedding.HasText


main :: IO ()
main = defaultMain [
  bgroup "train and save on text8"
    [ bench "1k"  $ nfIO (trainOnDataFile "data/text8s/text8_1k")
    , bench "10k"  $ nfIO (trainOnDataFile "data/text8s/text8_10k")
    , bench "100k" $ nfIO (trainOnDataFile "data/text8s/text8_100k")
    ]
    -- [ bench "1mb"  $ nfIO (trainOnDataFile "data/text8s/text8_1m")
    -- , bench "5mb"  $ nfIO (trainOnDataFile "data/text8s/text8_5m")
    -- , bench "10mb" $ nfIO (trainOnDataFile "data/text8s/text8_10m")
    -- , bench "25mb" $ nfIO (trainOnDataFile "data/text8s/text8_25m")
    -- ]
  ]
  where
    trainOnDataFile dataFilePath = saveModel =<< train =<< (setFilePath <$> getDataFileName dataFilePath)
    setFilePath inputFilePath = (Skipgram, learningDefault{ _input  = inputFilePath
                                                          , _output = inputFilePath <> ".out"})
