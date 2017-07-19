{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module WordEmbedding.HasText.Internal.Type
  (
  -- * API arguments
  -- $APIParams
    HasTextArgs
  , HasTextMethod(..)
  , HasTextOptions(..)
  , HasTextLoss(..)
  , HasTextResult(..)
  -- * Internal Parameters
  , Params(..)
  , LParams(..)
  , Model
  , WordVec
  , WordVecRef
  , TMap
  , RandomIO
  , Weights(..)
  , MWeights(..)
  , Dict(..)
  , Entry(..)
  ) where

import           Data.HashMap.Strict                      (HashMap)
import qualified Data.Vector.Unboxed                      as VU
import qualified Data.Vector.Unboxed.Mutable              as VUM
import           Data.Text                                (Text)
import           Data.Mutable
import qualified System.Random.MWC                        as RM
import qualified System.Random.MWC.CondensedTable         as RMC

import           GHC.Generics                             (Generic)
import           Control.Concurrent
import           Control.Monad.Reader

-- $APIParams
--
-- Types below are exposed as HasText's API. So These are added the package name as prefix.

-- | Arguments necessary to learn
type HasTextArgs = (HasTextMethod, HasTextOptions)

-- | Learning algorithms
data HasTextMethod = Cbow | Skipgram deriving (Show, Generic)

-- | Global options to learn
data HasTextOptions = HasTextOptions
  { _input          :: FilePath    -- ^ training file path
  , _output         :: FilePath    -- ^ output file path
  , _initLR         :: Double      -- ^ learning rate
  , _lrUpdateTokens :: Word        -- ^ the number of tokens that update the learning rate
  , _dim            :: Word        -- ^ size of word vectors
  , _windows        :: Word        -- ^ size of the context window
  , _epoch          :: Word        -- ^ number of epochs
  , _minCount       :: Word        -- ^ minimal number of word occurences
  , _negatives      :: Word        -- ^ number of negatives sampled
  , _lossFn         :: HasTextLoss -- ^ loss function {ns, hs}
  , _tSub           :: Double      -- ^ sub-sampling threshold
  , _threads        :: Word        -- ^ number of threads
  , _verbose        :: Word        -- ^ verbosity level
  } deriving (Show, Generic)

-- | Loss functions
data HasTextLoss = Negative | Hierarchical deriving (Show, Read, Generic)

-- | The result of Word2Vec method.
-- In contrast to LParams type, it is more preferable that each label of this record is lazy evoluted.
data HasTextResult = HasTextResult
  { htArgs          :: HasTextArgs
  , htDict          :: Dict
  , htSigf          :: Double -> Double          -- ^ (memorized) sigmoid function
  , htLogf          :: Double -> Double          -- ^ (memorized) log function
  , htNoiseDist     :: RMC.CondensedTableV Entry -- ^ noise distribution table
  , htWordVec       :: WordVec                   -- ^ word vectors
  }

-- | A parameter throughout learning. Params should be thread-safe since it is shared among threads.
data Params = Params
  { _args          :: HasTextArgs
  , _dict          :: {-# UNPACK #-} Dict
  , _lr            :: Double
  , _sig           :: Double -> Double             -- ^ (memorized) sigmoid function
  , _log           :: Double -> Double             -- ^ (memorized) log function
  , _noiseDist     :: RMC.CondensedTableV Entry    -- ^ noise distribution table
  , _wordVecRef    :: WordVecRef                   -- ^ word vectors
  , _tokenCountRef :: IORef Word                   -- ^ the number of tokens consumed
  }

-- | A local parameter per thread.
data LParams = LParams
  { _loss   :: IOURef Double
  , _hidden :: {-# UNPACK #-} (VUM.IOVector Double)
  , _grad   :: {-# UNPACK #-} (VUM.IOVector Double)
  , _rand   :: RM.GenIO
  }

type Model = ReaderT (Params, LParams) IO ()

type WordVec    = TMap Weights
type WordVecRef = MVar (TMap MWeights)

type TMap a = HashMap Text a
type RandomIO = IO

-- | The pair of input/output word vectors correspond to a word.
data Weights = Weights
  { _wI :: {-# UNPACK #-} (VU.Vector Double) -- ^ input word vector
  , _wO :: {-# UNPACK #-} (VU.Vector Double) -- ^ output word vector
  } deriving (Generic)

data MWeights = MWeights
  { _mwI :: {-# UNPACK #-} (VUM.IOVector Double) -- ^ input word vector
  , _mwO :: {-# UNPACK #-} (VUM.IOVector Double) -- ^ output word vector
  }

data Entry = Entry
  { _eWord  :: Text
  , _eCount :: Word
  -- , etype    :: EntryType
  -- , subwords :: ~(Vec T.Text)
  } deriving (Generic, Show)

-- data EntryType = EWord | ELabel deriving (Generic)
-- instance Binary EntryType

data Dict = Dict
  { _entries  :: TMap Entry
  , _discards :: TMap Double
  , _ntokens  :: Word
  } deriving (Generic, Show)
