module Helpers (removeDuplicates, textToByteString, showText) where

import Data.ByteString (ByteString)
import Data.Set (fromList, toList)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

textToByteString :: Text -> ByteString
textToByteString = encodeUtf8

showText :: (Show a) => a -> Text
showText = pack . show
