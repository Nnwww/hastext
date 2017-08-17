#!/usr/bin/env stack
{- stack --resolver lts-8.23 --install-ghc runghc
   --package FindBin --package http-conduit --package conduit-combinators --package bytestring
-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude                    hiding (FilePath)
import System.Environment
import System.Environment.FindBin
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = httpLBS "http://mattmahoney.net/dc/text8.zip" >>= L8.putStrLn
