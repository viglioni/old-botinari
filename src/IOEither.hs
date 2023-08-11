--
-- Functions related to the alias type IOEither
--
module IOEither
  ( IOEither
  , HttpRes
  , HttpResDefault
  , fromIO
  ) where

import Control.Exception (try)
import Data.ByteString.Lazy (ByteString)
import Exception (ErrorMsg, formatException)
import Network.HTTP.Client (HttpException)
import Network.Wreq (Response)

-- | IOEither a
-- Alias type for: IO (Either ErrorMsg a)
type IOEither a = IO (Either ErrorMsg a)

-- | HttpRes a
-- Alias type for: IO (Either HttpException (Response a))
type HttpRes a = IO (Either HttpException (Response a))

-- | IOEither a
-- Alias type for: IO (Either ErrorMsg a)
type HttpResDefault = HttpRes Data.ByteString.Lazy.ByteString

fromIO :: IO a -> IO (Either ErrorMsg a)
fromIO ioObj = formatException <$> try ioObj
