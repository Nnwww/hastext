{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit

import           TestDict
import           TestData

import           TextShow
import qualified Data.Text                   as T

import qualified WordEmbedding.HasText.Args  as HA
import qualified WordEmbedding.HasText       as H

slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

testNonFail :: Int -> T.Text -> HA.Args -> IO ()
testNonFail topn posWord a = do
  printT ("train start" :: T.Text)
  w <- H.train a
  printT ("train end" :: T.Text)
  printT ("mostSim start" :: T.Text)
  let Right rl = H.mostSimilar w [posWord] []
  let r        = slice 0 topn rl
  printT r
  printT ("saveModel start" :: T.Text)
  H.saveModel w
  printT ("saveVecCompat start" :: T.Text)
  H.saveVectorCompat w
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
            , testCase "A series of Hastext's operations is not fail" $ (testNonFail 10 "a" =<< noFailParams)
            , testCase "Hastext run on text8" $ (testNonFail 10 "may" =<< text8RunParams)
            ]
