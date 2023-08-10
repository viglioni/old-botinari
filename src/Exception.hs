{-# LANGUAGE OverloadedStrings #-}

module Exception (ErrorMsg, formatIOException, formatException, formatStatus, formatIOStatus) where

import Control.Arrow (left)
import Control.Exception (SomeException)
import Control.Lens ((^.))
import Data.Text (Text)
import Helpers (showText)
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException))
import Network.Wreq
  ( responseBody,
    responseHeaders,
    responseStatus, responseLink, linkURL, statusCode,
  )

type ErrorMsg = Text

formatHttpException :: HttpException -> ErrorMsg
formatHttpException (HttpExceptionRequest _ (StatusCodeException err _)) =
  "Caught Exception: Status:"
    <> showText (err ^. responseStatus)
    <> "Headers:"
    <> showText (err ^. responseHeaders)
    <> "Body:"
    <> showText (err ^. responseBody)
    <> "Link:"
    <> showText (err ^. responseLink "rel" "next" . linkURL)
formatHttpException e = "Caught Exception: " <> showText e

formatIOException :: IO (Either HttpException a) -> IO (Either ErrorMsg a)
formatIOException = fmap (left formatHttpException)

formatException :: Either SomeException a -> Either ErrorMsg a
formatException = left (("Caught Exception: " <>) . showText)


formatStatus :: HttpException -> Int
formatStatus (HttpExceptionRequest _ (StatusCodeException err _)) = err ^. responseStatus . statusCode
formatStatus _ = -1

formatIOStatus :: IO (Either HttpException a) -> IO (Either Int a)
formatIOStatus = fmap (left formatStatus)
