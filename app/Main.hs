{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Config (FishyConfig, readConfig)
import Control.Monad (void)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Shelly
import System.Console.CmdArgs.Implicit
import Weather (weatherFrom)

default (Text)

data Fishy
  = Mix {project :: FilePath, testPath :: Maybe Text, redis :: Bool, zedQL :: Bool}
  | Weather {cityName :: Text}
  deriving (Show, Data, Typeable)

configPath :: Maybe Text -> FilePath
configPath Nothing = ""
configPath (Just home) = fromText . T.concat $ [home, "/.fishy/conf.yaml"]

elixirSuite :: Fishy
elixirSuite =
  Mix
    { project = def &= help "Path to Elixir project",
      testPath = def &= help "Optional path to the test file",
      redis = def &= help "Whether to spawn a local Redis instance",
      zedQL = def &= help "Whether or not the current project is ZED QL so we can run other migrations first"
    }

weather :: Fishy
weather = Weather {cityName = "" &= help "The name of the city to get the weather of"}

runRedisServer :: Sh ()
runRedisServer = do
  echo "Running Redis server...\n"
  shelly . escaping False . silently $ run_ "redis-server" []

runMixSuite :: Fishy -> Sh Int
runMixSuite cArgs = do
  echo "Running mix test suite...\n"
  shelly . errExit False . verbosely $ do
    cd $ project cArgs
    run_ "mix" $ "test" : maybeToList (testPath cArgs) ++ ["--color"]

    lastExitCode

runElixirMigrations :: FilePath -> Text -> Sh ()
runElixirMigrations projectPath appName = do
  echo $ T.concat ["Running Elixir migrations for ", appName, "...\n"]
  shelly $ do
    setenv "MIX_ENV" "test"

    cd projectPath
    let (mix, ectoSetup) = mixEctoSetup
    run_ mix ectoSetup

mixEctoSetup :: (FilePath, [Text])
mixEctoSetup = let setupCommands = ["do", "ecto.drop,", "ecto.create,", "ecto.migrate"] in ("mix", setupCommands)

elixirProjectPath :: Maybe Text -> Text -> FilePath
elixirProjectPath (Just home) projectName = fromText . T.concat $ [home, "/Development/zed/", projectName]
elixirProjectPath Nothing projectName = fromText projectName

closeRedisServer :: Sh ()
closeRedisServer = do
  echo "Trying to kill Redis server...\n"
  shelly . silently $ do
    res <- run "pgrep" ["6379"]
    case T.unsnoc res of
      Just (pid, _) -> do
        echo "Killing Redis server...\n"
        run_ "kill" ["-s", "TERM", pid]
      Nothing -> return ()

handleElixirSuite :: Fishy -> Maybe Text -> Sh ()
handleElixirSuite cArgs home = do
  when (redis cArgs) $ do
    void $ asyncSh runRedisServer

  when (zedQL cArgs) $ do
    let (racing, zedApi) = ("racing-api", "zed-api")
    void $ runElixirMigrations (elixirProjectPath home zedApi) zedApi
    void $ runElixirMigrations (elixirProjectPath home racing) racing

  -- we're capturing the exit code here so we can do cleanup tasks even
  -- if the test suite fails
  suiteExitCode <- runMixSuite cArgs

  -- The redis server is started async and stays in the background so
  -- we're explicitly shutting it down
  when (redis cArgs) closeRedisServer

  exit suiteExitCode

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
