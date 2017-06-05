{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module WordEmbedding.HasText.Dict where

import           WordEmbedding.HasText.Args

import           System.IO                    as SI
import qualified System.Random.MWC            as RM

import qualified Data.Char                    as C
import qualified Data.HashMap.Strict          as HS
import qualified Data.ByteString              as BS
import qualified Data.Vector                  as V
import qualified Data.Text                    as T
import           Data.Conduit
import qualified Data.Conduit.Combinators     as CC

import qualified Data.Store                   as S
import           TH.Derive (Deriving, derive)

import Control.Monad

type TMap a = HS.HashMap T.Text a

data Entry = Entry
  { eword :: T.Text
  , count :: Word
  -- , etype    :: EntryType
  -- , subwords :: ~(Vec T.Text)
  }

data EntryType = EWord | ELabel

data Dict = Dict
  { entries  :: TMap Entry
  , discards :: TMap Double
  , ntokens  :: Word
  }

maxVocabSize = 30000000 :: Int
maxLineSize  =     1024 :: Int

-- deriving a binary serialization.
$($(derive [d|
    instance Deriving (S.Store Entry)
    |]))

$($(derive [d|
    instance Deriving (S.Store EntryType)
    |]))

$($(derive [d|
    instance Deriving (S.Store Dict)
    |]))

saveDict :: FilePath -> Dict -> IO ()
saveDict savePath dict = BS.writeFile savePath $ S.encode dict

loadDict :: FilePath -> IO Dict
loadDict readPath = S.decodeIO =<< BS.readFile readPath

initDiscards :: Double -> TMap Entry -> Word -> TMap Double
initDiscards tsub ents tks = HS.map calcDiscard ents
  where
    calcDiscard e =
      let f = realToFrac (count e) / realToFrac tks in
        (sqrt $ tsub / f) + (tsub / f)

-- |
-- The function folding words splited by @Data.Char.isSpace@ from a file.
-- Note that files as source is utf8 only.
wordsFromFile :: (r -> T.Text -> r) -> r -> FilePath -> IO r
wordsFromFile modifier plain readPath =
  runConduitRes $ CC.sourceFile readPath
  .| CC.decodeUtf8
  .| CC.splitOnUnboundedE C.isSpace
  .| CC.foldl modifier plain

-- |
-- The function that discard a word according to noise distribution.
-- I recommend applying threshold function to hash map at the 1st argment in advance because words that don't exist in hash map are also discarded.
discard :: TMap Double -> RM.GenIO -> T.Text -> IO Bool
discard diss gen word =
  case HS.lookup word diss of
    Nothing -> return False
    Just disProb -> do
      randProb <- RM.uniform gen
      return $ randProb > disProb

getLine :: Handle -> Dict -> RM.GenIO -> IO (V.Vector Entry)
getLine h dict rand = do
  size <- SI.hFileSize h
  when (size == 0) $ SI.hSeek h SI.AbsoluteSeek 0
  unsafeGetLine h dict rand

unsafeGetLine :: Handle -> Dict -> RM.GenIO -> IO (V.Vector Entry)
unsafeGetLine h (Dict{entries = ents, discards = diss}) rand =
  runConduit $ CC.sourceHandle h
    .| CC.decodeUtf8
    .| CC.takeWhileE (/= '\n')
    .| CC.splitOnUnboundedE C.isSpace
    .| CC.filterM (discard diss rand)
    .| CC.map (\w -> ents HS.! w)
    .| CC.sinkVector

initFromFile :: Args -> IO Dict
initFromFile (_, Options{input = inp, tSub = tsub, minCount = minc}) = do
  ents <- wordsFromFile addEntries HS.empty inp
  let newEnts = threshold ents minc
      newTkns = sizeTokens newEnts
      newDiss = initDiscards tsub newEnts newTkns in
    return $ Dict newEnts newDiss newTkns

threshold :: TMap Entry -> Word -> TMap Entry
threshold ents t = HS.filter (\e -> t > count e) ents
    -- Improvable?: if I use lazy IO, it can suppress explosion of memory usage here.

sizeTokens :: TMap Entry -> Word
sizeTokens ents = foldr (\e acc -> acc + count e) 0 ents

addEntries :: TMap Entry -> T.Text -> TMap Entry
addEntries ents t = HS.alter newEntry t ents
  where
    newEntry (Just old@Entry{count = c}) = Just $ old {count = succ c}
    newEntry Nothing = Just $ Entry {eword = t, count = 1}
    -- todo: implement ngram and label functionality
    -- nGrams n = (!! n) . L.transpose . L.map T.inits . T.tails
