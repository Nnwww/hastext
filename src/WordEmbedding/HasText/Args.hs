{-# LANGUAGE StrictData    #-}
{-# LANGUAGE DeriveGeneric #-}
module WordEmbedding.HasText.Args
  ( Args
  , Method(..)
  , Options(..)
  , Loss(..)
  , learningDefault
  , validOpts
  ) where

import qualified System.Directory as SD
import qualified System.FilePath  as SF

import qualified Data.Binary      as B
import           GHC.Generics     (Generic)

-- | Arguments necessary to learn
type Args = (Method, Options)

-- | Learning algorithms
data Method
  = Cbow
  | Skipgram
  deriving (Show, Generic)

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
  } deriving (Show, Generic)

-- | Loss functions
data Loss = Negative | Hierarchical deriving (Show, Read, Generic)

-- | default learning parameter based on FastText.
learningDefault :: Options
learningDefault = Options
  { input          = ""
  , output         = ""
  , lr             = 0.1
  , lrUpdateTokens = 100
  , dim            = 100
  , windows        = 5
  , epoch          = 1
  , minCount       = 1
  , negatives      = 5
  , loss           = Negative
  , tSub           = 0.0001
  , threads        = 8
  , verbose        = 1
  }

instance B.Binary Method
instance B.Binary Options
instance B.Binary Loss

validOpts :: Args -> IO Bool
validOpts (_, o) = fmap (&& nonZeroThread) existPaths
  where
    nonZeroThread = 0 /= threads o
    existPaths = do
      existIFile <- SD.doesFileExist $ input o
      existODir  <- SD.doesDirectoryExist . SF.takeDirectory $ output o
      return $ existIFile && existODir
