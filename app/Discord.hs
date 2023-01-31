{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord where

import GHC.Generics

import Data.Text

import Data.Aeson

data Author = Author {
    authorId :: Text,
    username :: Text,
    discriminator :: Text,
    avatar :: Text,
    publicFlags :: Int
} deriving (Show, Generic)

instance ToJSON Author where
  toJSON = genericToJSON defaultOptions

instance FromJSON Author where
    parseJSON = withObject "Author" $ \v -> Author
        <$> v.: "id"
        <*> v.: "username"
        <*> v.: "discriminator"
        <*> v.: "avatar"
        <*> v.: "public_flags"

data EmbedData = EmbedData {
    name :: Text,
    url :: Text
} deriving (Show, Generic)

instance FromJSON EmbedData where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON EmbedData where
  toJSON = genericToJSON defaultOptions

data Media = Media {
    height :: Int,
    width :: Int,
    mediaUrl :: Text
} deriving (Show, Generic)

instance FromJSON Media where
    parseJSON = withObject "Media" $ \v -> Media
        <$> v.: "height"
        <*> v.: "width"
        <*> v.: "url"

instance ToJSON Media where
  toJSON = genericToJSON defaultOptions

data Embed = Embed {
    embedtype :: Text,
    embedUrl :: Text,
    title :: Text,
    description :: Text,
    color :: Int,
    embedAuthor :: EmbedData,
    provider :: EmbedData,
    thumbnail :: Media,
    video :: Media
} deriving (Show, Generic)

instance FromJSON Embed where
    parseJSON = withObject "Embed" $ \v -> Embed
        <$> v.: "type"
        <*> v.: "url"
        <*> v.: "title"
        <*> v.: "description"
        <*> v.: "color"
        <*> v.: "author"
        <*> v.: "provider"
        <*> v.: "thumbnail"
        <*> v.: "video"

instance ToJSON Embed where
  toJSON = genericToJSON defaultOptions


data Discord = Discord {
    id :: Text,
    messageType :: Int,
    content :: Text,
    channelId :: Text,
    author :: Author,
    attachments :: [Text],
    embeds :: [Embed],
    pinned :: Bool,
    tts :: Bool,
    timestamp :: Text
} deriving (Show, Generic)


instance FromJSON Discord where
    parseJSON = withObject "Discord" $ \v -> Discord
        <$> v.: "id"
        <*> v.: "type"
        <*> v.: "content"
        <*> v.: "channel_id"
        <*> v.: "author"
        <*> v.: "attachments"
        <*> v.: "embeds"
        <*> v.: "pinned"
        <*> v.: "tts"
        <*> v.: "timestamp"

instance ToJSON Discord where
    toJSON = genericToJSON defaultOptions