{-# LANGUAGE OverloadedStrings #-}

module Config (FishyConfig (..), APIKey (..), readConfig) where

import Conduit (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Yaml as Y

data APIKey = APIKey {apiKeyName :: Text, apiKeyValue :: Text} deriving (Show)

newtype FishyConfig = FishyConfig {configApiKeys :: [APIKey]} deriving (Show)

instance FromJSON FishyConfig where
  parseJSON = withObject "FishyConfig" $ \obj -> do
    keys <- obj .: "api_keys"
    pure . FishyConfig . map (uncurry APIKey) . Map.toList $ keys

readConfig :: (MonadThrow m, MonadIO m) => FilePath -> m FishyConfig
readConfig confPath = liftIO (B.readFile confPath) >>= Y.decodeThrow