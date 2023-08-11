{-# LANGUAGE OverloadedStrings #-}

module Url
  ( uploadBlobUrl
  , createRecordUrl
  , createSessionUrl
  , artUrl
  , artInfoUrl
  , artPingUrl
  ) where

import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Env (envGet)
import IOEither (IOEither)

uploadBlobUrl :: String
uploadBlobUrl = "https://bsky.social/xrpc/com.atproto.repo.uploadBlob"

createRecordUrl :: String
createRecordUrl = "https://bsky.social/xrpc/com.atproto.repo.createRecord"

createSessionUrl :: String
createSessionUrl = "https://bsky.social/xrpc/com.atproto.server.createSession"

artInfoUrl :: Int -> IOEither String
artInfoUrl artNum =
  runExceptT $ do
    url <- ExceptT $ envGet "ART_INFO"
    return $ url <> show artNum <> "?lang=pt&size=full"

data Quality
  = Low
  | High

getQuality :: Quality -> String
getQuality Low = "250"
getQuality High = "2000"

_artUrl :: Quality -> Int -> IOEither String
_artUrl quality artNum =
  runExceptT $ do
    url <- ExceptT $ envGet "ART_PIC"
    return $ url <> getQuality quality <> "/" <> show artNum <> ".jpg"

artUrl :: Int -> IOEither String
artUrl = _artUrl High

artPingUrl :: Int -> IOEither String
artPingUrl = _artUrl Low
