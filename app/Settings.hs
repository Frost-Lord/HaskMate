module Settings 
    ( Settings (..)
    , loadSettings
    ) where

import Data.Aeson ((.:), (.:?), FromJSON(..), withObject, eitherDecode, (.!=))
import Data.String (fromString)
import qualified Data.ByteString.Lazy as BSL
import System.Directory (doesFileExist)

-- Data type for the settings
data Settings = Settings
  { ignore :: [String]
  , delay :: Maybe Int
  , script :: String
  , cmd :: String
  } deriving (Show)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \v ->
    Settings <$> v .:? (fromString "ignore") .!= []
             <*> v .:? (fromString "delay") .!= Nothing
             <*> v .:? (fromString "script") .!= ""
             <*> v .:? (fromString "cmd") .!= ""

-- Load the settings from a JSON file
loadSettings :: FilePath -> IO (Maybe Settings)
loadSettings path = do
  exists <- doesFileExist path
  if exists
    then do
      fileContent <- BSL.readFile path
      let result = eitherDecode fileContent :: Either String Settings
      case result of
        Left e -> putStrLn ("Error decoding settings: " ++ e) >> return Nothing
        Right settings -> return (Just settings)
    else return Nothing