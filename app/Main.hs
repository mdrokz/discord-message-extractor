{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where


import Discord (Author)

import System.Environment (getArgs)
import Data.Text
import Data.String.Interpolate (i)

-- Date Imports
import Data.Time
import Data.Aeson

version = "0.0.1"

help = "Usage: run --token <your discord token> --channel <discord channel> --limit <message limit for API response>"


-- method to substract x days from today
calculateDateTimeStamp :: NominalDiffTime -> UTCTime -> UTCTime
calculateDateTimeStamp x k = addUTCTime (-86400 * x) k
                            
                                                        
discordUrl :: Text -> Text -> Text -> String
discordUrl channel limit before  = [i|https://discordapp.com/api/v9/channels/#{channel}/messages?limit=#{limit}&before=#{before}|]

main :: IO ()
main = getArgs >>= \case 
  ["help"] -> putStrLn help
  ["run"] -> do
    t <- getCurrentTime
    let x = calculateDateTimeStamp 200 t
    --  x to string
    let y = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" x
    let c = formatTime defaultTimeLocale "%s" x
    let z = decode "{\"id\": \"248186636381192193\", \"username\": \"md\", \"display_name\": null, \"avatar\": \"7a09670f05c69416ab37b4aae4522965\", \"avatar_decoration\": null, \"discriminator\": \"9742\", \"public_flags\": 4194432}" :: Maybe Author
    print z
    putStrLn c
    putStrLn "Running!"
    putStrLn $ discordUrl "123" "123" "123"
  ["version"] -> putStrLn version
  _ -> do
    putStrLn "Hello, Haskell!"