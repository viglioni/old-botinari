{-# LANGUAGE OverloadedStrings #-}

module Exception
  ( ErrorMsg
  , formatIOException
  , formatException
  , formatStatus
  , formatIOStatus
  , throwError
  ) where

import Control.Arrow (left)
import Control.Exception (SomeException)
import Control.Lens ((^.))
import Data.Text (Text, unpack)
import Helpers (showText)
import Network.HTTP.Client
  ( HttpException(HttpExceptionRequest)
  , HttpExceptionContent(StatusCodeException)
  )
import Network.Wreq
  ( linkURL
  , responseBody
  , responseHeaders
  , responseLink
  , responseStatus
  , statusCode
  )

type ErrorMsg = Text

formatHttpException :: HttpException -> ErrorMsg
formatHttpException (HttpExceptionRequest _ (StatusCodeException err _)) =
  "Caught Exception: Status:" <>
  showText (err ^. responseStatus) <>
  "Headers:" <>
  showText (err ^. responseHeaders) <>
  "Body:" <>
  showText (err ^. responseBody) <>
  "Link:" <> showText (err ^. responseLink "rel" "next" . linkURL)
formatHttpException e = "Caught Exception: " <> showText e

formatIOException :: IO (Either HttpException a) -> IO (Either ErrorMsg a)
formatIOException = fmap (left formatHttpException)

formatException :: Either SomeException a -> Either ErrorMsg a
formatException = left (("Caught Exception: " <>) . showText)

formatStatus :: HttpException -> Int
formatStatus (HttpExceptionRequest _ (StatusCodeException err _)) =
  err ^. responseStatus . statusCode
formatStatus _ = -1

formatIOStatus :: IO (Either HttpException a) -> IO (Either Int a)
formatIOStatus = fmap (left formatStatus)

throwError :: ErrorMsg -> IO ()
throwError = error . unpack
