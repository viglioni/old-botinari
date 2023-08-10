{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Skeet (skeet)
import Configuration.Dotenv (loadFile, defaultConfig)

main :: IO ()
main = do
  loadFile defaultConfig 
  res <- skeet
  print res
