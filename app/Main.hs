{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

-- Date Imports

import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens
import Control.Monad
import Data.Aeson (ToJSON (toJSON), Value, decode, encode)
import qualified Data.ByteString as BStr
import Data.ByteString.Char8 as C8 (ByteString, pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Map as Map
import Data.String.Interpolate (i)
import Data.Text (pack, replace, unpack)
import Discord (Author, Discord (uId), Embed, Media, SimplifiedData (sId), toSimplifiedData)
import Network.Wreq
import System.Environment (getArgs)

version = "0.0.1"

help = "Usage: run -token <your discord token> -channel <discord channel> -limit <message limit for API response>"

stringToBS :: String -> ByteString
stringToBS str = do C8.pack str

discordUrl :: String -> String -> String
discordUrl channel limit = [i|https://discordapp.com/api/v9/channels/#{channel}/messages?limit=#{limit}|]

type DiscordRes = Response [Discord]

fetchData :: String -> String -> IO DiscordRes
fetchData token url = do
  let opts = defaults & header "Authorization" .~ [stringToBS token]
  asJSON =<< getWith opts url :: IO DiscordRes

-- processChunks :: [String] -> [Discord] -> ([String] -> IO [Discord]) -> IO [Discord]
-- processChunks [] [] _ = pure []
-- processChunks xs k t = do
--   case xs of
--     [] -> pure k
--     xs -> do
--       l <- t xs
--       processChunks (Prelude.drop 10 xs) l t

processChunks xs acc n lm t
  | n == 0 = pure acc
  | otherwise = do
      print n
      threadDelay 100000
      let l = Prelude.take 10 xs
      let remainingXs = Prelude.drop 10 xs
      (processedChunks, lastMessage) <- t l lm
      processChunks remainingXs (acc <> processedChunks) (n - 1) lastMessage t

processData token ul = do
  case ul of
    (x : xs) -> do
      r <- fetchData token x

      let d = r ^. responseBody

      let simplifiedDatas = fmap toSimplifiedData d

      let lastMessage = sId $ Prelude.last simplifiedDatas

      let limit = div (Prelude.length xs) 10

      processedDatas <- case limit of
        k | k > 3 -> do
          processChunks xs [] limit lastMessage $ \xxs s ->
            do
              (dl, id) <- process token s xxs []
              pure (dl, id)
        _ -> do
          (dl, _) <- process token lastMessage xs []
          pure dl

      -- processedDatas <- mapConcurrently (process token lastMessage) xs

      let t = d <> processedDatas

      -- pure $ fmap toSimplifiedData t
      pure t
  where
    process token id xs dl = do
      case xs of
        [] -> pure (dl, id)
        (x : xs) -> do
          print id
          r <- fetchData token $ x ++ "&before=" ++ id
          let d = r ^. responseBody
          case d of
            [] -> pure (dl, id)
            ld -> process token (uId $ Prelude.last ld) xs (dl <> ld)

main :: IO ()
main =
  getArgs >>= \case
    ["help"] -> putStrLn help
    ["run", "--token", t, "--channel", c, "--limit", l] -> do
      let lim = read l :: Int

      let messageLimit = div lim 100

      let threads = [1 .. messageLimit]

      let values = fmap (\_ -> discordUrl c "100") threads

      p <- processData t values

      let r = LBS.toStrict $ encode p

      BStr.writeFile "data.json" r
    ["version"] -> putStrLn version
    _ -> do
      putStrLn "Enter a valid command!"