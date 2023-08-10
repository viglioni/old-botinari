{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Skeet (skeet) where

import Blob (Blob, uploadBlob)
import Data.Aeson (ToJSON (toJSON), defaultOptions, fieldLabelModifier)
import Data.Aeson.TH (deriveToJSON)
import Data.Text (Text)
import GHC.Generics ( Generic )
import Network.Wreq (Response, postWith)
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT))
import Env (envGetText)
import IOEither (fromIO, IOEither)
import Time (getCurrentTime)
import ArtInfo (PostContent(PostContent), chooseArtNumber, getPostContent)
import Url (createRecordUrl)
import Auth (getHeaders)

data ImageType = ImageType
  { image :: Blob,
    alt :: Text
  }
  deriving (GHC.Generics.Generic, Show)

instance ToJSON ImageType

data Embed = Embed
  { _type :: Text,
    images :: [ImageType]
  }
  deriving (GHC.Generics.Generic, Show)

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier =
           let f "_type" = "$type"
               f other = other
            in f
       }
     ''Embed
 )

data RecordPost = RecordPost
  { text :: Text,
    createdAt :: Text,
    embed :: Maybe Embed
  }
  deriving (GHC.Generics.Generic, Show)

instance ToJSON RecordPost

data SkeetBody = SkeetBody
  { collection :: Text,
    repo :: Text,
    record :: RecordPost
  }
  deriving (GHC.Generics.Generic, Show)

instance ToJSON SkeetBody


reqBodyWithOnePic :: Text -> Text -> PostContent -> Blob -> SkeetBody
reqBodyWithOnePic did datetime (PostContent skeetText altText) blob =
  SkeetBody
    "app.bsky.feed.post"
    did
    ( RecordPost
        skeetText
        datetime
        ( Just
            ( Embed
                "app.bsky.embed.images"
                [ImageType blob altText]
            )
        )
    )


skeet :: IOEither (Response ByteString)
skeet = runExceptT $ do
  artNumber <- ExceptT  chooseArtNumber
  did <- ExceptT $ envGetText "DID"
  dateTime <- ExceptT $ fromIO getCurrentTime
  opts <-  ExceptT getHeaders
  blob <- ExceptT $ uploadBlob artNumber opts
  postContent <- ExceptT $ getPostContent artNumber
  let reqBody = toJSON $ reqBodyWithOnePic did dateTime postContent blob
  ExceptT $ fromIO $ postWith opts createRecordUrl reqBody



