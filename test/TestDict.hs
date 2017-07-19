{-# LANGUAGE OverloadedStrings #-}
module TestDict where

import WordEmbedding.HasText.Dict
import TestData

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS

import Test.Tasty.HUnit

instance Eq Entry where
  (==) Entry{_eWord = wa, _eCount = ca}
       Entry{_eWord = wb, _eCount = cb} = (wa == wb) && (ca == cb)

testAddEntries :: Assertion
testAddEntries = assert (assumption == testAEData)
  where
    testAEData = fadd "b" . fadd "a" . fadd "b" $ HS.empty
    fadd = flip addEntries
    assumption = HS.fromList [("a", Entry "a" 1), ("b", Entry "b" 2)]

listWordsFromFile :: FilePath -> IO [T.Text]
listWordsFromFile fp = foldWordsFromFile (\a t -> t : a) [] fp

testReadCorrectlyWordsFromFile :: Assertion
testReadCorrectlyWordsFromFile = assert $ do
  input <- listWordsFromFile =<< noFailPath
  return $ noFailInputList == L.reverse input

testCollectFromFile :: Assertion
testCollectFromFile = assert $ do
  ents <- foldWordsFromFile addEntries HS.empty =<< noFailPath
  return $ ents == assumption
    where
      assumption = HS.fromList . L.map (makekv . T.singleton) $ ['a' .. 'e']
      makekv w = (w, Entry w 5)

testInitFromFile :: Assertion
testInitFromFile = assert $ do
  dict@(Dict ents diss _) <- initFromFile =<< noFailParams
  -- print dict
  return $ HS.size ents /= 0 && HS.size diss /= 0
