#!/usr/bin/env stack
-- stack --resolver lts-8.21 --install-ghc runghc --package turtle --package FindBin
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.Environment
import System.Environment.FindBin
import Data.Maybe

checkRequiredCommands = do
  ewget <- which "wget"
  ecurl <- which "curl"
  getExecutablePath

main = do
  -- corpusPath <- stdin
  print =<< checkRequiredCommands
  print __Bin__
