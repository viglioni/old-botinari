{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Exception (throwError)
import Skeet (skeet)

main :: IO ()
main = do
  loadFile defaultConfig
  res <- skeet
  either throwError print res
