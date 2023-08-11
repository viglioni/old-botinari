module Http
  ( getStatus
  , getAndParse
  , postAndParse
  , get
  , post
  , get'
  , post'
  , ReqResponse
  ) where

import Control.Exception (try)
import Control.Lens (view)
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Exception (formatIOException, formatIOStatus)
import IOEither (IOEither, fromIO)
import Network.Wreq
  ( Options
  , Response
  , asJSON
  , defaults
  , postWith
  , responseBody
  , responseStatus
  , statusCode
  )
import qualified Network.Wreq as N

import Network.Wreq.Types (Postable)

type ResponseBody = ByteString

type ReqResponse = Response ResponseBody

-- | getStatus
-- Gets the statusCode from a get call
getStatus :: String -> IOEither Int
getStatus url = do
  status <-
    formatIOStatus (try $ view (responseStatus . statusCode) <$> N.get url)
  fromIO $ return (either id id status)

-- | get'
-- return the full response from get call
get' :: String -> IOEither ReqResponse
get' url = formatIOException $ try $ N.get url

-- | get
-- return the response body from a get call
get :: String -> IOEither ResponseBody
get url = formatIOException $ try $ view responseBody <$> N.get url

-- | get
-- get url and parse responseBody to a data type
getAndParse :: (FromJSON a) => String -> IOEither a
getAndParse url =
  formatIOException $ try $ view responseBody <$> (asJSON =<< N.get url)

-- | post'
-- return the full response of a post call
post' :: (Postable a) => String -> a -> Maybe Options -> IOEither ReqResponse
post' url payload options = formatIOException $ try $ postWith opts url payload
  where
    opts = fromMaybe defaults options

-- | post
-- post and return the response body of a post call
post :: (Postable a) => String -> a -> Maybe Options -> IOEither ResponseBody
post url payload options =
  formatIOException $ try $ view responseBody <$> postWith opts url payload
  where
    opts = fromMaybe defaults options

-- | postAndParse
-- post and parse responseBody to a data type
postAndParse ::
     (FromJSON b, Postable a) => String -> a -> Maybe Options -> IOEither b
postAndParse url payload options =
  formatIOException $
  try $ view responseBody <$> (asJSON =<< postWith opts url payload)
  where
    opts = fromMaybe defaults options
