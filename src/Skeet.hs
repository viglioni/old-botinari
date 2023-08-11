--
-- Functions related to skeeting (posting to blueksy)
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Skeet
  ( skeetArt
  ) where

import ArtInfo (PostContent(PostContent), chooseArtNumber, getPostContent)
import Auth (getHeaders)
import Blob (Blob, uploadBlob)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.Aeson (ToJSON(toJSON), defaultOptions, fieldLabelModifier)
import Data.Aeson.TH (deriveToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Env (envGetText)
import GHC.Generics (Generic)
import Http (ReqResponse, post')
import IOEither (IOEither, fromIO)
import Network.Wreq (Options, Response)
import Network.Wreq.Types (Postable)
import Time (getCurrentTime)
import Url (createRecordUrl)

data ImageType = ImageType
  { image :: Blob
  , alt :: Text
  } deriving (GHC.Generics.Generic, Show)

instance ToJSON ImageType

data Embed = Embed
  { _type :: Text
  , images :: [ImageType]
  } deriving (GHC.Generics.Generic, Show)

$(deriveToJSON
    defaultOptions
      { fieldLabelModifier =
          let f "_type" = "$type"
              f other = other
           in f
      }
    ''Embed)

data RecordPost = RecordPost
  { text :: Text
  , createdAt :: Text
  , langs :: [Text]
  , embed :: Maybe Embed
  } deriving (GHC.Generics.Generic, Show)

instance ToJSON RecordPost

data SkeetBody = SkeetBody
  { collection :: Text
  , repo :: Text
  , record :: RecordPost
  } deriving (GHC.Generics.Generic, Show)

instance ToJSON SkeetBody

reqBodyWithOnePic :: Text -> Text -> PostContent -> Blob -> SkeetBody
reqBodyWithOnePic did datetime (PostContent skeetText altText) blob =
  SkeetBody
    "app.bsky.feed.post"
    did
    (RecordPost
       skeetText
       datetime
       ["pt-BR"]
       (Just (Embed "app.bsky.embed.images" [ImageType blob altText])))

skeet :: Postable b => b -> Maybe Options -> IOEither ReqResponse
skeet = post' createRecordUrl

skeetArt :: IOEither (Response ByteString)
skeetArt =
  runExceptT $ do
    artNumber <- ExceptT chooseArtNumber
    did <- ExceptT $ envGetText "DID"
    dateTime <- ExceptT $ fromIO getCurrentTime
    opts <- ExceptT getHeaders
    blob <- ExceptT $ uploadBlob artNumber opts
    postContent <- ExceptT $ getPostContent artNumber
    let reqBody = toJSON $ reqBodyWithOnePic did dateTime postContent blob
    ExceptT $ skeet reqBody (Just opts)
