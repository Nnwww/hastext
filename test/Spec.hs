{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Test.Tasty
import Test.Tasty.HUnit

import           TestDict
import           TestData

import           Control.DeepSeq
import           Data.Monoid
import qualified Data.Text                   as T
import qualified System.Directory            as SD
import           WordEmbedding.HasText.Args
import           WordEmbedding.HasText

normalUseCase :: Word -> T.Text -> IO HasTextArgs -> ((String -> IO ()) -> Assertion)
normalUseCase topn posWord args step = do
  a@(_, HasTextOptions{_input = i, _output = o}) <- args
  step $ "input path: "  <> i
  step $ "output path: " <> o
  step "Running train"
  w <- train a
  step "Running mostSim"
  let Right r = mostSimilarN w topn [posWord] []
  step ("Top " <> show topn <> " of mostSimilar: " <> show r)
  step "Running saveModel"
  saveModel w
  let outputPath = _output . snd . htArgs $ w
  existanceOutputModel <- SD.doesFileExist outputPath
  assertEqual "save a model" True existanceOutputModel
  step "Running saveVecCompat"
  saveVectorCompat w
  existanceOutputVectorCompat <- SD.doesFileExist (outputPath <> ".vecc")
  assertEqual "Save vectors as a compatible form." True existanceOutputVectorCompat
  step "Running loadModel"
  Right _ <- loadModel (_output . snd $ a)
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
              (normalUseCase 10 "a" noFailOnMultiThreadParams)
            -- , testCaseSteps "Hastext run on text8" (noFailSteps 10 "may" text8RunParams)
            ]
