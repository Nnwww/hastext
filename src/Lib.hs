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

type Args = (Method, Options)

data Method
  = Cbow
  | Skipgram
  deriving Show

data Options = Options
  { input          :: FilePath
  , output         :: FilePath
  , lr             :: Double
  , lrUpdateTokens :: Int
  , dim            :: Int
  , windows        :: Int
  , epoch          :: Int
  , minCount       :: Int
  , negatives      :: Int
  , loss           :: Loss
  , tSub           :: Double
  , threads        :: Int
  , verbose        :: Int
  } deriving (Show)

-- Loss functions
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

train :: Args -> IO (FilePath)
train = undefined

saveArgs :: Args -> IO ()
saveArgs args@(_, opt) = BS.writeFile outFile $ encode args
  where
    outFile = output opt
