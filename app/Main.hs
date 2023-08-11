{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Exception (throwError)
import IOEither (IOEither)
import Network.HTTP.Client (Response)
import Skeet (skeetArt)
import System.Environment (getArgs)
import Text.Read (readMaybe)

data Arg
  = PostArt
  | PostLifeEvent
  deriving (Show, Enum, Read)

argErrorMsg :: String
argErrorMsg = "Arg should be one of: " <> argsList
  where
    argsList = show $ map show (enumFrom (toEnum 0) :: [Arg])

parseArgs :: [String] -> Arg
parseArgs [] = error argErrorMsg
parseArgs (x:_) = fromMaybe (error argErrorMsg) (readMaybe x :: Maybe Arg)

choosePost :: Arg -> IOEither (Response ByteString)
choosePost PostArt = skeetArt
choosePost PostLifeEvent = error "not implemented yet"

main :: IO ()
main = do
  loadFile defaultConfig
  command <- parseArgs <$> getArgs
  res <- choosePost command
  either throwError print res
