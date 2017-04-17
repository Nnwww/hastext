{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module WordEmbedding.FastText.Args
  ( Args
  , Method(..)
  , Options(..)
  , Loss(..)
  , learningDefault
  , train
  , saveArgs
  , readArgs
  ) where

import qualified Data.ByteString        as BS
import qualified Data.Store             as ST
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
  , lrUpdateTokens :: Word     -- ^ the number of tokens that update the learning rate
  , dim            :: Word     -- ^ size of word vectors
  , windows        :: Word     -- ^ size of the context window
  , epoch          :: Word     -- ^ number of epochs
  , minCount       :: Word     -- ^ minimal number of word occurences
  , negatives      :: Word     -- ^ number of negatives sampled
  , loss           :: Loss     -- ^ loss function {ns, hs}
  , tSub           :: Double   -- ^ sub-sampling threshold
  , threads        :: Word     -- ^ number of threads
  , verbose        :: Word     -- ^ verbosity level
  } deriving (Show)

-- | Loss functions
data Loss = Negative | Hierarchical deriving (Show, Read)

learningDefault :: Options
learningDefault = Options
  { input          = ""
  , output         = ""
  , lr             = 0.05
  , lrUpdateTokens = 100
  , dim            = 100
  , windows        = 5
  , epoch          = 5
  , minCount       = 5
  , negatives      = 5
  , loss           = Negative
  , tSub           = 0.0001
  , threads        = 12
  , verbose        = 1
  }

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

-- | Save Args
saveArgs :: FilePath -> Args -> IO ()
saveArgs savePath args = BS.writeFile savePath $ ST.encode args

-- | Read Args
readArgs :: FilePath -> IO (Args)
readArgs readPath = ST.decodeIO =<< BS.readFile readPath
