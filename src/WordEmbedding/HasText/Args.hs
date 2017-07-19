{-# LANGUAGE StrictData    #-}
{-# LANGUAGE DeriveGeneric #-}

module WordEmbedding.HasText.Args
  ( HasTextArgs
  , HasTextMethod(..)
  , HasTextOptions(..)
  , HasTextLoss(..)
  , learningDefault
  , validOpts
  ) where

import qualified System.Directory                    as SD
import qualified System.FilePath                     as SF
import           Data.Binary                         (Binary)
import           Control.Monad.IO.Class
import           WordEmbedding.HasText.Internal.Type
                 ( HasTextArgs
                 , HasTextMethod(..)
                 , HasTextOptions(..)
                 , HasTextLoss(..)
                 , HasTextMethod(..)
                 )

instance Binary HasTextMethod
instance Binary HasTextOptions
instance Binary HasTextLoss

-- | default learning parameter based on FastText.
learningDefault :: HasTextOptions
learningDefault = HasTextOptions
  { _input          = ""
  , _output         = ""
  , _initLR         = 0.1
  , _lrUpdateTokens = 100
  , _dim            = 100
  , _windows        = 5
  , _epoch          = 1
  , _minCount       = 1
  , _negatives      = 5
  , _lossFn         = Negative
  , _tSub           = 0.0001
  , _threads        = 8
  , _verbose        = 1
  }

validOpts :: MonadIO m => HasTextArgs -> m Bool
validOpts (_, o) = liftIO $
  let nonZeroThread = 0 /= _threads o
      existPaths = do
        existIFile <- SD.doesFileExist $ _input o
        existODir  <- SD.doesDirectoryExist . SF.takeDirectory $ _output o
        return $ existIFile && existODir
  in fmap (&& nonZeroThread) existPaths
