--
-- .env related functions
--
module Env
  ( envGet
  , envGetText
  , loadEnv
  ) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Text (Text, pack)
import IOEither (IOEither, fromIO)
import System.Environment (getEnv)

envGet :: String -> IOEither String
envGet key = fromIO (getEnv key)

envGetText :: String -> IOEither Text
envGetText = fmap (fmap pack) . envGet

loadEnv :: IO ()
loadEnv = loadFile defaultConfig
