{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (catch)
import Data.ByteString.Lazy (ByteString)
import Exception (throwError)
import GHC.Exception (SomeException)
import IOEither (IOEither)
import Network.HTTP.Client (Response)
import Skeet (skeet)
import System.Environment (getArgs)

data Arg
  = PostArt
  | PostTimeline
  deriving (Show, Enum, Read)

argErrorMsg :: String
argErrorMsg = "Arg should be one of: " <> argsList
  where
    argsList = show $ map show (enumFrom (toEnum 0) :: [Arg])

parseArgs :: [String] -> IO Arg
parseArgs [] = error argErrorMsg
parseArgs (arg:_) = return (read arg :: Arg) `catch` handler
  where
    handler :: SomeException -> IO Arg
    handler _ = error argErrorMsg

choosePost :: Arg -> IOEither (Response ByteString)
choosePost PostArt = skeet
choosePost PostTimeline = error "not implemented yet "

main :: IO ()
main = do
  loadFile defaultConfig
  command <- parseArgs =<< getArgs
  res <- choosePost command
  either throwError print res
