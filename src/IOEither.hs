module IOEither (IOEither, HttpRes, HttpResDefault, getAndParse, postAndFormat, getAndFormat, postAndParse, fromIO, getStatus) where

import Control.Exception (try)
import Control.Lens (view)
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Exception (ErrorMsg, formatException, formatIOException, formatIOStatus)
import Network.HTTP.Client (HttpException)
import Network.Wreq (Options, Response, asJSON, defaults, get, postWith, responseBody, responseStatus, statusCode)
import Network.Wreq.Types (Postable)

-- | IOEither a
-- Alias type for: IO (Either ErrorMsg a)
type IOEither a = IO (Either ErrorMsg a)

-- | HttpRes a
-- Alias type for: IO (Either HttpException (Response a))
type HttpRes a = IO (Either HttpException (Response a))

-- | IOEither a
-- Alias type for: IO (Either ErrorMsg a)
type HttpResDefault = HttpRes Data.ByteString.Lazy.ByteString

getStatus :: String -> IOEither Int
getStatus url = do
  status <- formatIOStatus (try $ view (responseStatus . statusCode) <$> get url)
  fromIO $ return (either id id status)

getAndFormat :: String -> IOEither ByteString
getAndFormat url =
  formatIOException $ try $ view responseBody <$> get url

getAndParse :: (FromJSON a) => String -> IOEither a
getAndParse url =
  formatIOException $ try $ view responseBody <$> (asJSON =<< get url)

postAndFormat ::
  (Postable a) =>
  String ->
  a ->
  Maybe Options ->
  IOEither Data.ByteString.Lazy.ByteString
postAndFormat url payload options =
  formatIOException $
    try $
      view responseBody
        <$> postWith opts url payload
  where
    opts = fromMaybe defaults options

postAndParse ::
  (FromJSON b, Postable a) =>
  String ->
  a ->
  Maybe Options ->
  IOEither b
postAndParse url payload options =
  formatIOException $
    try $
      view responseBody
        <$> (asJSON =<< postWith opts url payload)
  where
    opts = fromMaybe defaults options

fromIO :: IO a -> IO (Either ErrorMsg a)
fromIO ioObj = formatException <$> try ioObj
