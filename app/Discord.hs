{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord where

import Control.Lens (without)
import Data.Aeson
import Data.String
import GHC.Generics
import qualified Data.Maybe

data Author = Author
  { authorId :: String,
    username :: String,
    discriminator :: String,
    avatar :: String,
    publicFlags :: Int
  }
  deriving (Show, Generic)

instance ToJSON Author where
  toJSON = genericToJSON defaultOptions

instance FromJSON Author where
  parseJSON = withObject "Author" $ \v ->
    Author
      <$> v
      .: "id"
      <*> v
      .: "username"
      <*> v
      .: "discriminator"
      <*> v
      .: "avatar"
      <*> v
      .: "public_flags"

data EmbedData = EmbedData
  { name :: String,
    url :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON EmbedData where
  parseJSON = withObject "EmbedData" $ \v ->
    EmbedData
      <$> v
      .: "name"
      <*> v
      .:? "url"

instance ToJSON EmbedData where
  toJSON = genericToJSON defaultOptions

data Media = Media
  { height :: Int,
    width :: Int,
    mediaUrl :: String
  }
  deriving (Show, Generic)

instance FromJSON Media where
  parseJSON = withObject "Media" $ \v ->
    Media
      <$> v
      .: "height"
      <*> v
      .: "width"
      <*> v
      .: "url"

instance ToJSON Media where
  toJSON = genericToJSON defaultOptions

data Embed = Embed
  { embedType :: String,
    embedUrl :: String,
    title :: Maybe String,
    description :: Maybe String,
    color :: Maybe Int,
    embedAuthor :: Maybe EmbedData,
    provider :: Maybe EmbedData,
    thumbnail :: Maybe Media,
    video :: Maybe Media
  }
  deriving (Show, Generic)

instance FromJSON Embed where
  parseJSON = withObject "Embed" $ \v ->
    Embed
      <$> v
      .: "type"
      <*> v
      .: "url"
      <*> v
      .:? "title"
      <*> v
      .:? "description"
      <*> v
      .:? "color"
      <*> v
      .:? "author"
      <*> v
      .:? "provider"
      <*> v
      .:? "thumbnail"
      <*> v
      .:? "video"

-- specify optional field in json
-- https://stackoverflow.com/questions/44675309/optional-field-in-aeson

instance ToJSON Embed where
  toJSON = genericToJSON defaultOptions

data Attachment = Attachments
  { fId :: String,
    filename :: String,
    fDescription :: Maybe String
  }
  deriving (Show, Generic)


instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \v ->
    Attachments
      <$> v
      .: "id"
      <*> v
      .: "filename"
      <*> v
      .:? "description"
instance ToJSON Attachment where
  toJSON = genericToJSON defaultOptions


data Discord = Discord
  { uId :: String,
    messageType :: Int,
    content :: String,
    channelId :: String,
    author :: Author,
    attachments :: [Attachment],
    embeds :: [Embed],
    pinned :: Bool,
    tts :: Bool,
    timestamp :: String
  }
  deriving (Show, Generic)

instance FromJSON Discord where
  parseJSON = withObject "Discord" $ \v ->
    Discord
      <$> v
      .: "id"
      <*> v
      .: "type"
      <*> v
      .: "content"
      <*> v
      .: "channel_id"
      <*> v
      .: "author"
      <*> v
      .: "attachments"
      <*> v
      .: "embeds"
      <*> v
      .: "pinned"
      <*> v
      .: "tts"
      <*> v
      .: "timestamp"

instance ToJSON Discord where
  toJSON = genericToJSON defaultOptions

data SimplifiedData = SimplifiedData
  { sId :: String,
    sAuthor :: Maybe EmbedData,
    sContent :: String,
    sDescription :: Maybe String,
    sEmbed :: String,
    sTitle :: String,
    sThumbnail :: String,
    sTimestamp :: String
  }
  deriving (Show, Generic)

instance ToJSON SimplifiedData where
  toJSON (SimplifiedData i a c d e t th ts) =
    object
      [ "id" .= i,
        "author" .= a,
        "content" .= c,
        "description" .= d,
        "type" .= e,
        "title" .= t,
        "thumbnail" .= th,
        "timestamp" .= ts
      ]

toSimplifiedData discord =
  SimplifiedData
    { sId = uId discord,
      sAuthor = case embeds discord of
        [] -> Nothing
        x -> (embedAuthor . Prelude.head) x,
      sContent = content discord,
      sDescription = case embeds discord of
        [] -> Nothing
        x -> (description . Prelude.head) x,
      sEmbed = case embeds discord of
        [] -> ""
        x -> (embedType . Prelude.head) x,
      sTitle = case embeds discord of
        [] -> ""
        x -> Data.Maybe.fromMaybe "" ((title . head) x),
      sThumbnail = case embeds discord of
        [] -> ""
        x -> maybe "" mediaUrl ((thumbnail . Prelude.head) x),
      sTimestamp = timestamp discord
    }