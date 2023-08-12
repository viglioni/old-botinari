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
import Data.Text (Text, length, splitOn, take)
import GHC.Generics (Generic)
import Helpers (removeDuplicates, removeRx)
import Http (getAndParse, getStatus)
import IOEither (IOEither, fromIO)
import Prelude hiding (length, take)
import System.Random (randomRIO)
import Url (Lang(..), artInfoUrl, artPingUrl)

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

getAltText :: Art -> Art -> Text
getAltText artPt artEn = memo_descricao artPt <> "\n\n" <> memo_descricao artEn

formatYear :: Art -> Text
formatYear = take 4 . removeRx "[^0-9]" . txt_data_exibicao

formatCollection :: Art -> Art -> Text
formatCollection artPt artEn = "\n@: " <> colPt <> " / " <> colEn
  where
    colPt = colecao artPt
    colEn = colecao artEn

formatArtType :: Art -> Art -> Text
formatArtType artPt artEn = "\n" <> kindPt <> " / " <> kindEn
  where
    kindPt = removeRx "(\r\n|\n|\r)" . tipo_obra $ artPt
    kindEn = removeRx "(\r\n|\n|\r)" . tipo_obra $ artEn

_formatMeasure :: [Measure] -> Text
_formatMeasure [] = ""
_formatMeasure measures = mconcat ["\n", height, "cm x ", width, "cm"]
  where
    height = altura . head $ measures
    width = largura . head $ measures

formatMeasures :: Art -> Text
formatMeasures = _formatMeasure . medidas

formatTheme :: Art -> Text
formatTheme =
  ("\n#: " <>) .
  mconcat .
  intersperse ", " .
  removeDuplicates . splitOn ":" . mconcat . intersperse ":" . map tema . temas

formatTechnique :: Art -> Art -> Text
formatTechnique artPt artEn = "\n" <> getTech artPt <> " / " <> getTech artEn
  where
    getTech =
      mconcat . intersperse ", " . removeDuplicates . map tecnica . tecnicas

formatTitle :: Art -> Art -> Text
formatTitle artPt artEn = txt_titulo artPt <> "\n" <> txt_titulo artEn <> "\n"

_composePost :: Art -> Art -> Text
_composePost artPt artEn =
  formatTitle artPt artEn <>
  formatYear artPt <>
  "\n" <>
  formatCollection artPt artEn <>
  formatArtType artPt artEn <>
  formatTechnique artPt artEn <> formatMeasures artPt <> "\n"

addTheme :: Art -> Text -> Text
addTheme artInfo basePost =
  if length post <= 300
    then post
    else basePost
  where
    themes = formatTheme artInfo
    post = basePost <> themes

composePost :: Art -> Art -> Text
composePost artPt artEn =
  addTheme artEn . addTheme artPt $ _composePost artPt artEn

getArtInfo :: Int -> IOEither (Art, Art)
getArtInfo artNum =
  runExceptT $ do
    urlEn <- ExceptT $ artInfoUrl EN artNum
    urlPt <- ExceptT $ artInfoUrl BR artNum
    infoPt <- ExceptT (getAndParse urlPt :: IOEither Art)
    infoEn <- ExceptT (getAndParse urlEn :: IOEither Art)
    return (infoPt, infoEn)

getPostContent :: Int -> IOEither PostContent
getPostContent artNum =
  runExceptT $ do
    (artInfoPt, artInfoEn) <- ExceptT $ getArtInfo artNum
    return $ postContent artInfoPt artInfoEn
  where
    postContent infoPt infoEn =
      PostContent (composePost infoPt infoEn) (getAltText infoPt infoEn)

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
