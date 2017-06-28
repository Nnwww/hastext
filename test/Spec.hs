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


testNonFail :: IO ()
testNonFail = do
  a <- noFailParams
  printTL ("train start" :: T.Text)
  w <- H.train a
  printTL ("train end" :: T.Text)
  printTL ("mostSim start" :: T.Text)
  let Right r = H.mostSimilar w ["a"] []
  printTL r
  printTL ("saveModel start" :: T.Text)
  H.saveModel w
  printTL ("saveVecCompat start" :: T.Text)
  H.saveVectorCompat w
  printTL ("loadModel start" :: T.Text)
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
            , testCase "A series of Hastext's operations is not fail" $ testNonFail
            ]
