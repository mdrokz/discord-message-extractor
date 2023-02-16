{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Main where

-- Date Imports

import Control.Lens

import Control.Concurrent.Async (async, wait)

import Control.Monad
import Data.Aeson (Value, decode, encode, ToJSON (toJSON))
import Data.ByteString.Char8 as C8 (ByteString, pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Map as Map
import Data.String.Interpolate (i)
import Data.Text (pack,replace,unpack)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Discord (Author, Discord, Embed, Media , SimplifiedData,toSimplifiedData)
import Network.Wreq
import System.Environment (getArgs)
import qualified Data.ByteString as BStr

version = "0.0.1"

help = "Usage: run -token <your discord token> -channel <discord channel> -limit <message limit for API response>"

stringToBS :: String -> ByteString
stringToBS str = do C8.pack str

removeChar :: Char -> String -> String
removeChar c s = unpack (replace (Data.Text.pack [c]) mempty (Data.Text.pack s))

-- method to substract x days from today
calculateDateTimeStamp d k =  addUTCTime (-86400 * d) k

processTimestamps :: [UTCTime] -> UTCTime -> [UTCTime]
processTimestamps [] _ = []
processTimestamps (x:xs) k = calculateDateTimeStamp 100 k : (processTimestamps xs $ calculateDateTimeStamp 100 k)


discordUrl :: String -> String -> String
discordUrl channel limit = [i|https://discordapp.com/api/v9/channels/#{channel}/messages?limit=#{limit}|]

type DiscordRes = Response [Discord]

fetchData :: String -> String -> IO DiscordRes
fetchData token url = do
  let opts = defaults & header "Authorization" .~ [stringToBS token]
  asJSON =<< getWith opts url :: IO DiscordRes

processData t u = do
  r <- fetchData t u
  -- discord data to simplified data using fmap
  let d = r ^. responseBody

  let simplifiedDatas = fmap toSimplifiedData d

  print "processing data\n"
  print u

  pure simplifiedDatas

main :: IO ()
main =
  getArgs >>= \case
    ["help"] -> putStrLn help
    ["run", "--token", t, "--channel", c, "--limit", l] -> do
      -- Ki.scoped \scope -> do 
      --   Ki.
      -- r <- fetchData t $ discordUrl c l

      let lim = read l :: Int

      let messageLimit = div lim 100

      let threads = [1 .. messageLimit]

      currentTime <- getCurrentTime

      -- let d = discordUrl c 100
      -- let gh = calculateDateTimeStamp 100 currentTime
      -- let g = d ++ gh :: String

      let actions = fmap (const currentTime) threads

      --  removeChar 's' $ removeChar '.' $

      let timestamps =  removeChar 's' . removeChar '.' . show . utcTimeToPOSIXSeconds <$> processTimestamps actions currentTime

      -- print timestamps

      let values = discordUrl c "100" : fmap (\x -> discordUrl c "100" ++ "&before=" ++ x) timestamps

      threads <- mapM (async . processData t) values

      results <- mapM wait threads

      putStrLn $ "Array length: " ++ show (Prelude.length results)

      let p = Prelude.take 12 $ Prelude.head results

      let r = LBS.toStrict $ encode p

      -- write r to a json file
      BStr.writeFile "data.json" r

      -- print $ Prelude.head results
      print r

      -- putStrLn $ "Array length: " ++ show (Prelude.length results)

      -- Ki.scoped \scope -> do
      --     currentTime <- getCurrentTime

      --     pure ()

      -- a <- getArgs
      print t
      print c
      print l

      t <- getCurrentTime
      let x = calculateDateTimeStamp 100 t
      --  x to string
      -- 1064197824746623047
      -- use formatTime to get long unix epoch

      let z = decode "{\"id\": \"248186636381192193\", \"username\": \"md\", \"display_name\": null, \"avatar\": \"7a09670f05c69416ab37b4aae4522965\", \"avatar_decoration\": null, \"discriminator\": \"9742\", \"public_flags\": 4194432}" :: Maybe Author
      print z
      print x
      putStrLn "Running!"
      putStrLn $ discordUrl "123" "123"
    ["version"] -> putStrLn version
    _ -> do
      putStrLn "Hello, Haskell!"