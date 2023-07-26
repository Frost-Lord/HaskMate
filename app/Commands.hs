{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Commands (displayHelpData, displayVersionData) where

import Network.HTTP.Simple
import Network.HTTP.Types.Header (hUserAgent)
import Data.ByteString.Lazy.Char8 ()
import Data.Aeson
import GHC.Generics
import Data.Text as T hiding (length, replicate)

newtype Release = Release { tag_name :: T.Text } deriving (Show, Generic)

instance FromJSON Release where
  parseJSON = withObject "Release" $ \v -> Release <$> v .: "tag_name"

-- ANSI escape sequences for color
red, white, yellow, green :: String
red = "\x1b[31m"
white = "\x1b[37m"
yellow = "\x1b[33m"
green = "\x1b[32m"

displayHelpData :: IO ()
displayHelpData = do
  let message = "Welcome to HaskMate!"
      boxWidth = length message + 4
      horizontalLine = red ++ replicate (boxWidth - 2) '\x2500'
      padding = replicate ((boxWidth - length message -1) `div` 2) ' '

  putStrLn $ red ++ "┌" ++ horizontalLine ++ "┐"
  putStrLn $ red ++ "│" ++ padding ++ green ++ message ++ red ++ padding ++ "│"
  putStrLn $ red ++ "└" ++ horizontalLine ++ "┘"
  putStrLn $ yellow ++ "Arguments:"
  putStrLn $ white ++ "  scriptPath  Path to the Haskell script you want to monitor"
  putStrLn $ green ++ "Example:"
  putStrLn $ white ++ "  haskmate app/Main.hs"

displayVersionData :: IO ()
displayVersionData = do
  putStrLn $ yellow ++ "Checking for updates..."
  initialRequest <- parseRequest "https://api.github.com/repos/Frost-Lord/HaskMate/releases/latest"

  let request = setRequestHeader hUserAgent ["HaskMate"] initialRequest

  response <- httpLBS request

  let responseBody = getResponseBody response

  -- putStrLn $ "Response Body: " ++ LBS.unpack responseBody --dev testing (ignore)

  let maybeRelease = eitherDecode responseBody :: Either String Release
  case maybeRelease of
    Right release -> do
      let currentVersion = "v1.1.0"
      if T.unpack (tag_name release) == currentVersion
        then putStrLn $ green ++ "You are using the latest version of HaskMate! " ++ currentVersion ++ white
        else putStrLn $ red ++ "A new version of HaskMate is available: " ++ T.unpack (tag_name release) ++ white
    Left err -> putStrLn $ red ++ "Failed to parse response body as Release: " ++ err ++ white
