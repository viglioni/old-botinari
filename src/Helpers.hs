{-# LANGUAGE OverloadedStrings #-}

module Helpers
  ( removeDuplicates
  , textToByteString
  , showText
  , removeRx
  , prettyPrint
  ) where

import Data.ByteString (ByteString)
import Data.Set (fromList, toList)
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Regex (mkRegex, subRegex)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

textToByteString :: Text -> ByteString
textToByteString = encodeUtf8

showText :: (Show a) => a -> Text
showText = pack . show

removeRx :: String -> Text -> Text
removeRx rx txt = pack $ subRegex (mkRegex rx) (unpack txt) ""

prettyPrint :: Text -> IO ()
prettyPrint = mapM_ print . splitOn "\n"
