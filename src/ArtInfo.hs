--
-- Functions related to getting and parsing the info for a certain art
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ArtInfo
  ( PostContent(PostContent)
  , getPostContent
  , chooseArtNumber
  ) where

import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.Aeson (FromJSON)
import Data.List (intersperse)
import Data.Text (Text, pack, splitOn, unpack)
import GHC.Generics (Generic)
import Helpers (removeDuplicates)
import Http (getAndParse, getStatus)
import IOEither (IOEither, fromIO)
import System.Random (randomRIO)
import Text.Regex (mkRegex, subRegex)
import Url (artInfoUrl, artPingUrl)

data Theme = Theme
  { tema :: Text
  } deriving (Show, Generic)

instance FromJSON Theme

data Measure = Measure
  { altura :: Text
  , largura :: Text
  } deriving (Show, Generic)

instance FromJSON Measure

data Technique = Technique
  { tecnica :: Text
  } deriving (Show, Generic)

instance FromJSON Technique

data Colour = Colour
  { cor :: Text
  } deriving (Show, Generic)

instance FromJSON Colour

data Art = Art
  { id :: Text
  , txt_titulo :: Text
  , txt_data_exibicao :: Text
  , tipo_obra :: Text
  , colecao :: Text
  , memo_descricao :: Text
  , temas :: [Theme]
  , medidas :: [Measure]
  , tecnicas :: [Technique]
  , cores :: [Colour]
  } deriving (Show, Generic)

instance FromJSON Art

data PostContent = PostContent
  { postText :: Text
  , altText :: Text
  } deriving (Show)

getAltText :: Art -> Text
getAltText = memo_descricao

formatYear :: Art -> Text
formatYear art = pack $ take 4 $ subRegex rx year ""
  where
    rx = mkRegex "[^0-9]"
    year = unpack . txt_data_exibicao $ art

formatCollection :: Art -> Text
formatCollection = ("\nColeção: " <>) . colecao

formatArtType :: Art -> Text
formatArtType = ("\nTipo da obra: " <>) . tipo_obra

_formatMeasure :: [Measure] -> Text
_formatMeasure [] = ""
_formatMeasure measures = mconcat ["\nMedidas: ", height, "cm x ", width, "cm"]
  where
    height = altura . head $ measures
    width = largura . head $ measures

formatMeasures :: Art -> Text
formatMeasures = _formatMeasure . medidas

formatTheme :: Art -> Text
formatTheme =
  ("\nTemas: " <>) .
  mconcat .
  intersperse ", " .
  removeDuplicates . splitOn ":" . mconcat . intersperse ":" . map tema . temas

formatTechnique :: Art -> Text
formatTechnique =
  ("\nTénicas: " <>) .
  mconcat . intersperse ", " . removeDuplicates . map tecnica . tecnicas

composePost :: Art -> Text
composePost art =
  txt_titulo art <>
  "\n" <>
  formatYear art <>
  "\n" <>
  formatCollection art <>
  formatArtType art <>
  formatMeasures art <> formatTheme art <> formatTechnique art

getArtInfo :: Int -> IOEither Art
getArtInfo artNum =
  runExceptT $ do
    url <- ExceptT $ artInfoUrl artNum
    ExceptT (getAndParse url :: IOEither Art)

getPostContent :: Int -> IOEither PostContent
getPostContent artNum =
  runExceptT $ do
    artInfo <- ExceptT $ getArtInfo artNum
    return $ postContent artInfo
  where
    postContent info = PostContent (composePost info) (getAltText info)

artNumIsInvalid :: Int -> IOEither Bool
artNumIsInvalid artNum =
  runExceptT $ do
    url <- ExceptT $ artPingUrl artNum
    status <- ExceptT $ getStatus url
    return (status /= 200)

chooseArtNumber :: IOEither Int
chooseArtNumber =
  runExceptT $ do
    num <- ExceptT $ fromIO (randomRIO (1, 5788) :: IO Int)
    ExceptT $ fromIO $ print $ "Art number: " <> show num
    imgDoesntExists <- ExceptT $ artNumIsInvalid num
    if imgDoesntExists
      then ExceptT chooseArtNumber
      else return num
