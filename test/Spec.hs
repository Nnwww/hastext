{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit

import           TestDict
import           TestData

import           Data.Monoid
import qualified Data.Text                   as T
import qualified System.Directory            as SD
import qualified WordEmbedding.HasText.Args  as HA
import qualified WordEmbedding.HasText       as H

noFailSteps :: Word -> T.Text -> IO HA.Args -> ((String -> IO ()) -> Assertion)
noFailSteps topn posWord args step = do
  a <- args
  step "Running train"
  w <- H.train a
  step "Running mostSim"
  let Right r = H.mostSimilarN w topn [posWord] []
  step ("Top " <> show topn <> " of mostSimilar: " <> show r)
  step "Running saveModel"
  H.saveModel w
  let outputPath = HA.output . snd . H._args $ w
  existanceOutputModel <- SD.doesFileExist outputPath
  assertEqual "save a model" True existanceOutputModel
  step "Running saveVecCompat"
  H.saveVectorCompat w
  existanceOutputVectorCompat <- SD.doesFileExist (outputPath <> ".vecc")
  assertEqual "Save vectors as a compatible form." True existanceOutputVectorCompat
  step "Running loadModel"
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
            , testCaseSteps "A series of Hastext's operations is not fail (on multi thread)"
              (noFailSteps 10 "a" noFailOnMultiThreadParams)
            , testCaseSteps "Hastext run on text8" (noFailSteps 10 "may" text8RunParams)
            ]
