{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
module WordEmbedding.HasText.Internal.Strict.Model
  ( binaryLogistic
  , computeHidden
  , getmWI
  ) where

import           Control.Monad.Reader
import           Control.Concurrent
import qualified Data.Text                                        as T
import           Data.Hashable                                    (Hashable)
import qualified Data.HashMap.Strict                              as HS
import qualified Data.Vector                                      as V
import qualified Data.Vector.Unboxed.Mutable                      as VUM
import qualified Data.Mutable                                     as M
import qualified WordEmbedding.HasText.Internal.Strict.MVectorOps as HMV
import           WordEmbedding.HasText.Internal.Type
                 ( Params(..)
                 , LParams(..)
                 , Model
                 , MWeights(..)
                 , WordVecRef
                 )

-- |
-- The function that update model based on formulas of the objective function and binary label.
binaryLogistic :: Bool   -- ^ label in Xin's tech report. (If this is True function compute about positive word. If False, negative-sampled word.)
               -> T.Text -- ^ a updating target word
               -> Model
binaryLogistic label input = do
  (Params{..}, LParams{..}) <- ask
  liftIO $ do
    ws <- takeMVar _wordVecRef
    let mwo = _mwO (ws HS.! input)
    score <- fmap _sig (HMV.sumDotMM mwo _hidden)
    let alpha = _lr * (boolToNum label - score)
    HMV.foriM_ _grad (\i e -> do
                       emwo <- VUM.unsafeRead mwo i
                       return $ e + alpha * emwo)
    HMV.mapi (const (alpha *)) _hidden
    HMV.addMM mwo _hidden
    putMVar _wordVecRef ws
    let minusLog = negate . _log $! if label then score else 1.0 - score
    M.modifyRef' _loss (+ minusLog)
  where
    boolToNum = fromIntegral . fromEnum

computeHidden :: MonadIO m => VUM.IOVector Double -> WordVecRef -> V.Vector T.Text -> m ()
computeHidden hidden wsRef input = liftIO $ do
  ws <- readMVar wsRef
  mapM_ (HMV.addMM hidden) $ V.map (getmWI ws) input
  HMV.scale invLen hidden
  where
    inverse d = 1.0 / fromIntegral d
    invLen = inverse . V.length $! input

getmWI :: (Hashable k, Eq k) => HS.HashMap k MWeights -> k -> VUM.IOVector Double
getmWI w k = _mwI $! w HS.! k
