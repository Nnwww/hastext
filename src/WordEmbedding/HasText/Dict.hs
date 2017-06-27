{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE DeriveGeneric #-}

module WordEmbedding.HasText.Dict where

import           WordEmbedding.HasText.Args
import           WordEmbedding.HasText.Type

import           System.IO                  as SI
import qualified System.Random.MWC          as RM

import qualified Data.Char                  as C
import           Data.Conduit
import qualified Data.Conduit.Combinators   as CC
import qualified Data.HashMap.Strict        as HS
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import           Data.Binary
import           GHC.Generics (Generic)

import           Control.Monad

type TMap a = HS.HashMap T.Text a

data Entry = Entry
  { eword :: T.Text
  , count :: Word
  -- , etype    :: EntryType
  -- , subwords :: ~(Vec T.Text)
  } deriving (Generic)

data EntryType = EWord | ELabel deriving (Generic)

data Dict = Dict
  { entries  :: TMap Entry
  , discards :: TMap Double
  , ntokens  :: Word
  } deriving (Generic)

instance Binary Entry
instance Binary EntryType
instance Binary Dict

initDiscards :: Double -> TMap Entry -> Word -> TMap Double
initDiscards tsub ents tks = HS.map calcDiscard ents
  where
    calcDiscard e = sqrt (tsub / f e) + (tsub / f e)
    f e = realToFrac (count e) / realToFrac tks

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

getLineLoop :: Handle -> Dict -> RM.GenIO -> IO (V.Vector Entry)
getLineLoop h dict rand = do
  isE <- SI.hIsEOF h
  when isE $ SI.hSeek h SI.AbsoluteSeek 0
  unsafeGetLine h dict rand

unsafeGetLine :: Handle -> Dict -> RM.GenIO -> IO (V.Vector Entry)
unsafeGetLine h Dict{entries = ents, discards = diss} rand =
  runConduit $ CC.sourceHandle h
    .| CC.decodeUtf8
    .| CC.take 1024 -- TODO: move a constant to args
    .| CC.takeWhileE (/= '\n')
    .| CC.splitOnUnboundedE C.isSpace
    .| CC.filterM (discard diss rand)
    .| CC.map (ents HS.!)
    .| CC.sinkVector

initFromFile :: Args -> IO Dict
initFromFile (_, Options{input = inp, tSub = tsub, minCount = minc}) = do
  ents <- wordsFromFile addEntries HS.empty inp
  let newEnts = threshold ents minc
      newTkns = sizeTokens newEnts
      newDiss = initDiscards tsub newEnts newTkns
  return $ Dict newEnts newDiss newTkns

threshold :: TMap Entry -> Word -> TMap Entry
threshold ents t = HS.filter (\e -> t > count e) ents
    -- Improvable?: if I use lazy IO, it can suppress explosion of memory usage here.

sizeTokens :: TMap Entry -> Word
sizeTokens = foldr (\e acc -> acc + count e) 0

addEntries :: TMap Entry -> T.Text -> TMap Entry
addEntries ents t = HS.alter newEntry t ents
  where
    newEntry (Just old@Entry{count = c}) = Just old{count = succ c}
    newEntry Nothing                     = Just Entry{eword = t, count = 1}
    -- todo: implement ngram and label functionality
    -- nGrams n = (!! n) . L.transpose . L.map T.inits . T.tails
