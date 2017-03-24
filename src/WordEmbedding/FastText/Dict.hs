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
  , count    :: Word
  , etype    :: EntryType
  -- , subwords :: ~(Vec WordId)
  }

data EntryType = EWord | ELabel

data Dict = Dict
  { entries  :: TMap Entry
  , discards :: TMap Double
  , ntokens  :: Word
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

initDiscards :: Double -> TMap Entry -> Word -> TMap Double
initDiscards tsub ents tks = M.map calcDiscard ents
  where
    calcDiscard e =
      let f = realToFrac (count e) / realToFrac tks in
        (sqrt $ tsub / f) + (tsub / f)


initFromFile :: Args -> IO Dict
initFromFile (_, Options {input = inp, tSub = tsub, minCount = minc}) = do
  ents <- runConduitRes $ wordsFromFile addEnts M.empty inp
  let (newEnts, newTks) = threshold minc ents
      newDis = initDiscards tsub newEnts newTks in
    return $ Dict newEnts newDis newTks
  where
    wordsFromFile modifier plain readPath =
      CC.sourceFile readPath
      .| CC.decodeUtf8
      .| CC.splitOnUnboundedE C.isSpace
      .| CC.foldl modifier plain

hashMod :: T.Text -> Int -> Int
hashMod str bound = H.hash str `mod` bound

threshold :: Word -> TMap Entry -> (TMap Entry, Word)
threshold t ents = (newEnts, sizeTokens ents)
  where
    newEnts = M.filter (\e -> t > count e) ents -- Improvable?: if lazy IO, it can suppress explosion of memory usage here.

sizeTokens :: TMap Entry -> Word
sizeTokens ents = foldr (\e acc -> acc + count e) 0 ents

addEnts :: TMap Entry -> T.Text -> TMap Entry
addEnts ents t = M.alter newEntry t ents
  where
    newEntry (Just (old@Entry {count = c})) = Just $ old {count = succ c}
    newEntry Nothing = Just $ Entry {eword = t, count = 1, etype = EWord}
    -- todo: implement ngram and label functionality
    -- nGrams n = (!! n) . L.transpose . L.map T.inits . T.tails
