{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit

import           TestDict

import           TextShow
import qualified Data.Text                   as T

import Paths_hastext
import qualified WordEmbedding.HasText.Args  as HA
import qualified WordEmbedding.HasText       as H

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

testNonfail = do
  w2vSequence "a" =<< nonFail
  return ()
  where
    w2vSequence posw a = do
      printT ("train start" :: T.Text)
      w <- H.train a
      printT ("train end" :: T.Text)
      printT ("mostSim start" :: T.Text)
      let Right r = H.mostSimilar w [posw] []
      printT r
      printT ("saveModel start" :: T.Text)
      H.saveModel w
      printT ("saveVecCompat start" :: T.Text)
      H.saveVectorCompat w
      printT ("loadModel start" :: T.Text)
      !(l) <- H.loadModel (HA.output . snd $ a)
      return ()
    nonFail = do
      inputFilePath <- getDataFileName "data/NonFail.txt"
      return (HA.Skipgram, HA.learningDefault{ HA.input  = inputFilePath
                                             , HA.output = inputFilePath ++ ".out"
                                             })

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "A series of Hastext's operations is not fail" $ testNonfail
  ]
