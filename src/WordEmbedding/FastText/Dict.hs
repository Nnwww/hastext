{-# LANGUAGE StrictData #-}

module WordEmbedding.FastText.Dict where

import WordEmbedding.FastText.Args
import Data.Word
import Data.Vector

data Entry = Entry
  { word     :: String
  , count    :: Word64
  , etype    :: EntryType
  , subwords :: Vector WordId
  }

data EntryType = EWord | ELabel

data Dict = Dict
  { args     :: Args
  , word2Id  :: Vector WordId
  , entries  :: Vector Entry
  , pDiscard :: Vector Double
  , size     :: Word32
  , nwords   :: Word32
  , nlabels  :: Word32
  , ntokens  :: Word64
  }

type WordId = Maybe Word32
