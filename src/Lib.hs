module Lib
    ( Command(..)
    , Options(..)
    , Loss(..)
    , train
    ) where

data Command
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

train :: Command -> IO (FilePath)
train = undefined
