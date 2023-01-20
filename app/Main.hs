{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)

version = "0.0.1"

help = "Usage: run --token <your discord token> --channel <discord channel> --limit <message limit for API response>"

main :: IO ()
main = getArgs >>= \case 
  ["help"] -> putStrLn help
  ["run"] -> do
    putStrLn "Running!"
  ["version"] -> putStrLn version
  _ -> do
    putStrLn "Hello, Haskell!"