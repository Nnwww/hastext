{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}

module WordEmbedding.HasText.Args
  ( HasTextArgs(..)
  , HasTextMethod(..)
  , HasTextLoss(..)
  , learningDefault
  , validArgs
  ) where

import           Control.Monad.IO.Class
import           Data.Binary                         (Binary)
import qualified System.Directory                    as SD
import qualified System.FilePath                     as SF
import           WordEmbedding.HasText.Internal.Type
                 ( HasTextArgs (..)
                 , HasTextLoss (..)
                 , HasTextMethod (..)
                 , HasTextMethod (..)
                 )

instance Binary HasTextMethod
instance Binary HasTextArgs
instance Binary HasTextLoss

-- | default learning parameter based on FastText.
learningDefault :: HasTextArgs
learningDefault = HasTextArgs
  { _input          = ""
  , _output         = ""
  , _initLR         = 0.1
  , _lrUpdateTokens = 100
  , _dim            = 100
  , _windows        = 5
  , _epoch          = 1
  , _minCount       = 1
  , _negatives      = 5
  , _method         = Skipgram
  , _lossFn         = Negative
  , _tSub           = 0.0001
  , _threads        = 8
  , _verbose        = 1
  }

validArgs :: MonadIO m => HasTextArgs -> m Bool
validArgs args = liftIO $
  let nonZeroThread = 0 /= _threads args
      existPaths = do
        existIFile <- SD.doesFileExist $ _input args
        existODir  <- SD.doesDirectoryExist . SF.takeDirectory $ _output args
        return $ existIFile && existODir
  in fmap (&& nonZeroThread) existPaths
