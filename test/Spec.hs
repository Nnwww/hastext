{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit

import           TestDict
import           TestData

import           Data.Monoid
import           TextShow
import qualified Data.Text                   as T

import qualified WordEmbedding.HasText.Args  as HA
import qualified WordEmbedding.HasText       as H

import qualified System.Directory as SD


testNonFail :: Word -> T.Text -> HA.Args -> IO ()
testNonFail topn posWord a = do
  printT ("train start" :: T.Text)
  w <- H.train a
  printT ("train end" :: T.Text)
  printT ("mostSim start" :: T.Text)
  let Right r = H.mostSimilarN w topn [posWord] []
  printT r
  printT ("saveModel start" :: T.Text)
  H.saveModel w
  let outputPath = HA.output . snd . H._args $ w
  True <- SD.doesFileExist outputPath
  printT ("saveVecCompat start" :: T.Text)
  H.saveVectorCompat w
  True <- SD.doesFileExist (outputPath <> ".vecc")
  printT ("loadModel start" :: T.Text)
  !(_) <- H.loadModel (HA.output . snd $ a)
  return ()

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [ testCase "addEntries add entries" $ testAddEntries
            , testCase "wordsFromFile read a file" $ testReadCorrectlyWordsFromFile
            , testCase "(wordsFromFile addEntries) collect entries from file" $ testCollectFromFile
            , testCase "testInitFromFile is non zero" $ testInitFromFile
            -- , testCase "A series of Hastext's operations is not fail" $ (testNonFail 10 "a" =<< noFailParams)
            , testCase "A series of Hastext's operations is not fail (on multi thread)"
              (testNonFail 10 "a" =<< noFailOnMultiThreadParams)
            , testCase "Hastext run on text8" (testNonFail 10 "may" =<< text8RunParams)
            ]
