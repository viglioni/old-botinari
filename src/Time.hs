module Time
  ( getCurrentTime
  ) where

import Data.Text (Text, pack)
import qualified Data.Time as T
import Data.Time.Format.ISO8601 (iso8601Show)

getCurrentTime :: IO Text
getCurrentTime = pack . iso8601Show <$> T.getCurrentTime
