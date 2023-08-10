{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Blob (uploadBlob, Blob) where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Aeson (FromJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text (Text)
import Exception (ErrorMsg)
import GHC.Generics (Generic)
import IOEither (IOEither, getAndFormat, postAndParse)
import Network.Wreq (Options)
import Url (artUrl, uploadBlobUrl)

data Ref = Ref
  { _link :: Text
  }
  deriving (Generic, Show)

$( deriveFromJSON
     defaultOptions
       { fieldLabelModifier =
           let f "_link" = "$link"
               f other = other
            in f
       }
     ''Ref
 )

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier =
           let f "_link" = "$link"
               f other = other
            in f
       }
     ''Ref
 )

data Blob = Blob
  { _type :: Text,
    ref :: Ref,
    mimeType :: Text,
    size :: Int
  }
  deriving (Generic, Show)

$( deriveFromJSON
     defaultOptions
       { fieldLabelModifier =
           let f "_type" = "$type"
               f other = other
            in f
       }
     ''Blob
 )

$( deriveToJSON
     defaultOptions
       { fieldLabelModifier =
           let f "_type" = "$type"
               f other = other
            in f
       }
     ''Blob
 )

data ResponseBlob = ResponseBlob
  { blob :: Blob
  }
  deriving (Show, Generic)

instance FromJSON ResponseBlob

getPainting :: Int -> IOEither ByteString
getPainting artNum = runExceptT $ do
  url <- ExceptT $ artUrl artNum
  ExceptT $ getAndFormat url

uploadBlob :: Int -> Options -> IO (Either ErrorMsg Blob)
uploadBlob artNum opts = runExceptT $ do
  img <- ExceptT $ getPainting artNum
  responseBlob <-
    ExceptT
      ( postAndParse uploadBlobUrl img (Just opts) :: IOEither ResponseBlob
      )
  return $ blob responseBlob
