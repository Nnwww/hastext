{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WordEmbedding.HasText.Dict
  ( TMap
  , Dict(..)
  , Entry(..)
  , initDiscards
  , getLineLoop
  , unsafeGetLine
  , initFromFile
  , addEntries
  , foldWordsFromFile
  ) where

import           System.IO                           as SI
import qualified System.Random.MWC                   as RM
import           Data.Binary                         (Binary)
import           Data.Binary.Orphans                 ()
import qualified Data.Char                           as C
import           Data.Conduit
import qualified Data.Conduit.Combinators            as CC
import qualified Data.HashMap.Strict                 as HS
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception.Safe
import           WordEmbedding.HasText.Internal.Type
                 ( HasTextArgs
                 , HasTextArgs(..)
                 , TMap
                 , Dict(..)
                 , Entry(..)
                 )

instance Binary Entry
instance Binary Dict

initDiscards :: Double -> TMap Entry -> Word -> TMap Double
initDiscards tsub ents tks = HS.map calcDiscard ents
  where
    calcDiscard e = sqrt (tsub / f e) + (tsub / f e)
    f e = realToFrac (_eCount e) / realToFrac tks

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
unsafeGetLine h Dict{..} rand =
  runConduit $ CC.sourceHandle h
    .| CC.decodeUtf8
    .| CC.take 1024 -- TODO: move a constant to args
    .| CC.takeWhileE (/= '\n')
    .| CC.splitOnUnboundedE C.isSpace
    .| CC.filterM (discard _discards rand)
    .| CC.map (_entries HS.!)
    .| CC.sinkVector

-- |
-- The function folding words splited by @Data.Char.isSpace@ from a file.
-- Note that files as source is utf8 only.
foldWordsFromFile :: (r -> T.Text -> r) -> r -> FilePath -> IO r
foldWordsFromFile modifier plain readPath =
  runConduitRes $ CC.sourceFile readPath
  .| CC.decodeUtf8
  .| CC.splitOnUnboundedE C.isSpace
  .| CC.foldl modifier plain

initFromFile  :: HasTextArgs -> IO Dict
initFromFile HasTextArgs{..} = do
  ents <- foldWordsFromFile addEntries HS.empty _input
  let newEnts = threshold ents _minCount
      newTkns = sizeTokens newEnts
      newDiss = initDiscards _tSub newEnts newTkns
  when (newTkns == 0) $ throwString "To read file was success, but the dictionary did't have any remaining tokens after preprocessing. Your input parameters (e.g. minCount, tSub etc.) look again carefully."
  return $ Dict newEnts newDiss newTkns

threshold :: TMap Entry -> Word -> TMap Entry
threshold ents t = HS.filter (\e -> t < _eCount e) ents
    -- Improvable?: if I use lazy IO, it can suppress explosion of memory usage here.

sizeTokens :: TMap Entry -> Word
sizeTokens = foldr (\e acc -> acc + _eCount e) 0

addEntries :: TMap Entry -> T.Text -> TMap Entry
addEntries ents t = HS.alter newEntry t ents
  where
    newEntry (Just old@Entry{_eCount = c}) = Just old{_eCount = c + 1}
    newEntry Nothing                       = Just Entry{_eWord = t, _eCount = 1}
    -- todo: implement ngram and label functionality
    -- nGrams n = (!! n) . L.transpose . L.map T.inits . T.tails
