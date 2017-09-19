{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData       #-}

module WordEmbedding.HasText.Internal.Type
  (
  -- * API arguments
  -- $APIParams
    HasTextArgs(..)
  , HasTextMethod(..)
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

import           Data.HashMap.Strict              (HashMap)
import           Data.Mutable
import           Data.Text                        (Text)
import qualified Data.Vector.Unboxed              as VU
import qualified Data.Vector.Unboxed.Mutable      as VUM
import qualified System.ProgressBar               as P
import qualified System.Random.MWC                as RM
import qualified System.Random.MWC.CondensedTable as RMC

import           Control.Concurrent
import           Control.Monad.Reader
import           GHC.Generics                     (Generic)

-- $APIParams
--
-- Types below are exposed as HasText's API. So These are added the package name as prefix.

-- | Arguments necessary to learn
data HasTextArgs = HasTextArgs
  { _input          :: FilePath      -- ^ training file path
  , _output         :: FilePath      -- ^ output file path
  , _initLR         :: Double        -- ^ learning rate
  , _lrUpdateTokens :: Word          -- ^ the number of tokens that update the learning rate
  , _dim            :: Word          -- ^ size of word vectors
  , _windows        :: Word          -- ^ size of the context window
  , _epoch          :: Word          -- ^ number of epochs
  , _minCount       :: Word          -- ^ minimal number of word occurences
  , _negatives      :: Word          -- ^ number of negatives sampled
  , _method         :: HasTextMethod -- ^ learning method {Cbow|SkipGram}
  , _lossFn         :: HasTextLoss   -- ^ loss functions {Negative|Hierarchical}
  , _tSub           :: Double        -- ^ sub-sampling threshold
  , _threads        :: Word          -- ^ number of threads
  , _verbose        :: Word          -- ^ verbosity level
  } deriving (Show, Generic)

-- | Learning algorithms
data HasTextMethod = Cbow | Skipgram deriving (Show, Read, Generic)

-- | Loss functions
data HasTextLoss = Negative | Hierarchical deriving (Show, Read, Generic)

-- | The result of Word2Vec method.
-- In contrast to LParams type, it is more preferable that each label of this record is lazy evoluted.
data HasTextResult = HasTextResult
  { htArgs      :: HasTextArgs
  , htDict      :: Dict
  , htNoiseDist :: RMC.CondensedTableV Entry -- ^ noise distribution table
  , htWordVec   :: WordVec                   -- ^ word vectors
  }

-- | A parameter throughout learning. Params should be thread-safe since it is shared among threads.
data Params = Params
  { _args          :: {-# UNPACK #-} HasTextArgs   -- ^ user setting
  , _dict          :: {-# UNPACK #-} Dict          -- ^ dict of input corpus.
  , _noiseDist     :: RMC.CondensedTableV Entry    -- ^ noise distribution table
  , _wordVecRef    :: {-# UNPACK #-} WordVecRef    -- ^ word vectors
  , _tokenCountRef :: {-# UNPACK #-} (IORef Word)  -- ^ the number of tokens consumed
  , _progressRef   :: P.ProgressRef
  }

-- | A local parameter per thread.
data LParams = LParams
  { _loss   :: {-# UNPACK #-} (IOURef Double)
  , _lr     :: Double
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
