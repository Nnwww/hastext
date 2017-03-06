{-# LANGUAGE TemplateHaskell #-}

module Lib (Args
          , Method(..)
          , Options(..)
          , Loss(..)
          , train
          , saveArgs
          ) where

import qualified Data.ByteString as BS
import           Data.Store (Store, encode)
import           TH.Derive (Deriving, derive)

-- | Arguments necessary to learn
type Args = (Method, Options)

-- | Learning algorithms
data Method
  = Cbow
  | Skipgram
  deriving Show

-- | Global options to learn
data Options = Options
  { input          :: FilePath -- ^ training file path
  , output         :: FilePath -- ^ output file path
  , lr             :: Double   -- ^ learning rate
  , lrUpdateTokens :: Int      -- ^ the number of tokens that update the learning rate
  , dim            :: Int      -- ^ size of word vectors
  , windows        :: Int      -- ^ size of the context window
  , epoch          :: Int      -- ^ number of epochs
  , minCount       :: Int      -- ^ minimal number of word occurences
  , negatives      :: Int      -- ^ number of negatives sampled
  , loss           :: Loss     -- ^ loss function {ns, hs}
  , tSub           :: Double   -- ^ sub-sampling threshold
  , threads        :: Int      -- ^ number of threads
  , verbose        :: Int      -- ^ verbosity level
  } deriving (Show)

-- | Loss functions
data Loss = Negative | Hierarchical deriving (Show, Read)

-- derive Store instances at compile time
$($(derive [d|
    instance Deriving (Store Method)
    |]))

$($(derive [d|
    instance Deriving (Store Options)
    |]))

$($(derive [d|
    instance Deriving (Store Loss)
    |]))

-- | Start training
train :: Args -> IO (FilePath)
train = undefined

-- | Save Args
saveArgs :: Args -> IO ()
saveArgs args@(_, opt) = BS.writeFile outFile $ encode args
  where
    outFile = output opt
