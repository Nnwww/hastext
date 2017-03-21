{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module WordEmbedding.FastText.Dict where

import           WordEmbedding.FastText.Args

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST
import           Control.Monad.Trans.Resource
import           Control.Applicative

import qualified Data.Char                    as C
import           Data.Word
import qualified Data.List                    as L
import qualified Data.Map.Strict              as M
import           Data.Maybe
import qualified Data.ByteString              as BS
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as VUM
import qualified Data.Vector.Generic.Mutable  as VGM
import qualified Data.Text                    as T
import qualified Data.Store                   as S
import qualified Data.Hashable                as H
import           Data.Conduit
import qualified Data.Conduit.Combinators     as CC
import           TH.Derive (Deriving, derive)


type WordId    = Int
type TMap a    = M.Map T.Text a
type Vec a     = V.Vector a
type MVec s a  = VM.MVector s a
type UVec a    = VU.Vector a
type UMVec s a = VUM.MVector s a
type TrnsMV prim v a = prim (v (PrimState prim) a)

data Entry = Entry
  { eword    :: T.Text
  , count    :: Word64
  , etype    :: EntryType
  , subwords :: ~(Vec WordId)
  }

data EntryType = EWord | ELabel

data Dict = Dict
  { entries  :: Vec Entry
  , size     :: Word32
  , nwords   :: Word32
  , nlabels  :: Word32
  , ntokens  :: Word64
  }

maxVocabSize = 30000000 :: Int
maxLineSize  =     1024 :: Int

eos = "</s>" :: T.Text
bow =    "<" :: T.Text
eow =    ">" :: T.Text

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

initDiscards :: Dict -> Double -> Vec Entry -> Vec Double
initDiscards dict tsub entries = undefined

initNGrams :: Word32 -> V.Vector Entry -> V.Vector WordId
initNGrams size entries = undefined

wordsFromFile :: (MonadResource m, PrimMonad p, VGM.MVector v a)
              => (TrnsMV p v a -> T.Text -> TrnsMV p v a)
              -> TrnsMV p v a
              -> FilePath
              -> ConduitM i T.Text m (TrnsMV p v a)
wordsFromFile modifier init readPath = CC.sourceFile readPath
  .| CC.decodeUtf8
  .| CC.splitOnUnboundedE C.isSpace
  .| CC.foldl modifier init

initFromFile :: Args -> FilePath -> (Dict, MVec s WordId, MVec s Double)
initFromFile args readPath = undefined
  where
    --word2Id   = VM.replicate maxVocabSize Nothing :: TrnsSTV s WordId
    --pDiscards = VUM.replicate maxVocabSize 0      :: TrnsUSTV s Double

hashMod :: T.Text -> Int -> Int
hashMod str bound = H.hash str `mod` bound

threshold :: Word64 -> Word64 -> TMap Entry -> TMap Entry
threshold = undefined

add :: Dict -> TMap Entry -> T.Text -> (Dict, TMap Entry)
add dic entries' t = (dic {ntokens = succ $ ntokens dic}, updatedMap)
  where
    updatedMap = M.alter newEntry t entries'
    newEntry (Just (old@Entry {count = c})) = Just $ old {count = succ c}
    newEntry Nothing = Just $ Entry {eword = t, count = 1, etype = EWord, subwords = undefined}
    -- todo: implement ngram and label functionality
    -- nGrams n = (!! n) . L.transpose . L.map T.inits . T.tails
