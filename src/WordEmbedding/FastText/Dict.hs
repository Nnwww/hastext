{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module WordEmbedding.FastText.Dict where

import           WordEmbedding.FastText.Args
import           Data.Word
import           Data.Maybe
import           Data.ByteString              as BS
import           Data.Vector                  as V
import qualified Data.Text                    as T
import qualified Data.Store                   as ST
import           TH.Derive (Deriving, derive)

data Entry = Entry
  { word     :: String
  , count    :: Word64
  , etype    :: EntryType
  , subwords :: Vector WordId
  }

data EntryType = EWord | ELabel


data Dict = Dict
  { entries  :: Vector Entry
  , size     :: Word32
  , nwords   :: Word32
  , nlabels  :: Word32
  , ntokens  :: Word64
  }

type WordId = Maybe Word32

maxVocabSize = 30000000 :: Int
maxLineSize  =     1024 :: Int

eos = "</s>" :: T.Text
bow =    "<" :: T.Text
eow =    ">" :: T.Text

$($(derive [d|
    instance Deriving (ST.Store Entry)
    |]))

$($(derive [d|
    instance Deriving (ST.Store EntryType)
    |]))

$($(derive [d|
    instance Deriving (ST.Store Dict)
    |]))

singleton :: Args -> Dict
singleton args' = Dict
  { entries   = V.empty
  , size      = 0
  , nwords    = 0
  , nlabels   = 0
  , ntokens   = 0
  }

-- word2Id   = V.replicate maxVocabSize Nothing
-- pDiscards = V.empty

saveDict :: FilePath -> Dict -> IO ()
saveDict savePath dict = BS.writeFile savePath $ ST.encode dict

initDiscards :: Dict -> Double -> Vector Entry -> Vector Double
initDiscards dict tsub entries = undefined

initNGrams :: Word32 -> Vector Entry -> Vector WordId
initNGrams size entries = undefined

readArgs :: FilePath -> IO (Args)
readArgs readPath = ST.decodeIO =<< BS.readFile readPath

loadDict :: FilePath -> IO (Dict)
loadDict readPath = ST.decodeIO =<< BS.readFile readPath
