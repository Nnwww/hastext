module WordEmbedding.HasText.Type where

import qualified Data.HashMap.Strict as HS
import           Data.Hashable       (Hashable)
import qualified Data.Binary         as B

instance (Hashable k, Eq k, B.Binary k, B.Binary v) => B.Binary (HS.HashMap k v) where
  get = fmap HS.fromList B.get
  put = B.put . HS.toList
