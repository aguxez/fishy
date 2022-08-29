{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Main (main) where

import CLIArgs (Fishy (..), elixirSuite, weather)
import Config (FishyConfig, readConfig)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Elixir (handleElixirSuite)
import Shelly
import System.Console.CmdArgs.Implicit
import Weather (weatherFrom)

default (Text)

configPath :: Maybe Text -> FilePath
configPath Nothing = ""
configPath (Just home) = fromText . T.concat $ [home, "/.fishy/conf.yaml"]

handleWeather :: Fishy -> FishyConfig -> Sh ()
handleWeather cArgs config = do
  weatherRes <- liftIO . weatherFrom config . encodeUtf8 . cityName $ cArgs
  echo weatherRes

main :: IO ()
main = do
  cArgs <- cmdArgs (modes [elixirSuite, weather])
  shelly . silently $ do
    home <- get_env "HOME"
    let configPath' = configPath home
    configExists <- test_f configPath'

    unless configExists $ do
      echo "Config file does not exist..."
      exit 1

    config <- readConfig configPath'

    case cArgs of
      (Mix {}) -> handleElixirSuite cArgs home
      (Weather _) -> handleWeather cArgs config
