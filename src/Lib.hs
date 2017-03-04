{-# LANGUAGE TemplateHaskell #-}

module Lib (Args(..)
          , Options(..)
          , Loss(..)
          , train
          ) where

import Data.Store
import TH.Derive

data Args
  = Cbow Options
  | Skipgram Options
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

$($(derive [d|
    instance Deriving (Store Args)
    |]))

$($(derive [d|
    instance Deriving (Store Options)
    |]))

$($(derive [d|
    instance Deriving (Store Loss)
    |]))

train :: Args -> IO (FilePath)
train = undefined
