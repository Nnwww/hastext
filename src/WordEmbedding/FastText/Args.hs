{-# LANGUAGE TemplateHaskell #-}

module WordEmbedding.FastText.Args (Args
          , Method(..)
          , Options(..)
          , Loss(..)
          , train
          , getOptions
          , saveArgs
          , readArgs
          ) where

import qualified Data.ByteString        as BS
import qualified Data.Store             as ST
import qualified Control.Exception.Safe as ES
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
  { input          :: !FilePath -- ^ training file path
  , output         :: !FilePath -- ^ output file path
  , lr             :: !Double   -- ^ learning rate
  , lrUpdateTokens :: !Int      -- ^ the number of tokens that update the learning rate
  , dim            :: !Int      -- ^ size of word vectors
  , windows        :: !Int      -- ^ size of the context window
  , epoch          :: !Int      -- ^ number of epochs
  , minCount       :: !Int      -- ^ minimal number of word occurences
  , negatives      :: !Int      -- ^ number of negatives sampled
  , loss           :: !Loss     -- ^ loss function {ns, hs}
  , tSub           :: !Double   -- ^ sub-sampling threshold
  , threads        :: !Int      -- ^ number of threads
  , verbose        :: !Int      -- ^ verbosity level
  } deriving (Show)

-- | Loss functions
data Loss = Negative | Hierarchical deriving (Show, Read)

-- derive Store instances at compile time
$($(derive [d|
    instance Deriving (ST.Store Method)
    |]))

$($(derive [d|
    instance Deriving (ST.Store Options)
    |]))

$($(derive [d|
    instance Deriving (ST.Store Loss)
    |]))

-- | Start training
train :: Args -> IO (FilePath)
train = undefined

getOptions :: Args -> Options
getOptions (_, opts) = opts

-- | Save Args
saveArgs :: FilePath -> Args -> IO ()
saveArgs savePath args = BS.writeFile savePath $ ST.encode args

-- | Read Args
readArgs :: FilePath -> IO (Args)
readArgs readPath = ST.decodeIO =<< BS.readFile readPath
