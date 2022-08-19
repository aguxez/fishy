{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Control.Monad (void)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Shelly
import System.Console.CmdArgs.Implicit

default (T.Text)

data Fishy = ElixirProject {project :: FilePath, testPath :: Maybe Text, withRedis :: Bool, isZedQL :: Bool} deriving (Show, Data, Typeable)

elixirSuite :: Fishy
elixirSuite =
  ElixirProject
    { project = def &= help "Path to Elixir project",
      testPath = def &= help "Optional path to the test file",
      withRedis = def &= help "Whether to spawn a local Redis instance",
      isZedQL = def &= help "Whether or not the current project is ZED QL so we can run other migrations first"
    }

runRedisServer :: Sh ()
runRedisServer = do
  echo "Running Redis Server..."
  shelly $
    silently $ do
      run_ "redis-server" []

runMixSuite :: Fishy -> Sh ()
runMixSuite cArgs = do
  echo "Running mix test suite..."
  shelly $
    verbosely $ do
      cd $ project cArgs
      run_ "mix" $ "test" : maybeToList (testPath cArgs) ++ ["--color"]

runElixirMigrations :: FilePath -> Text -> Sh ()
runElixirMigrations projectPath appName = do
  echo $ T.concat ["Running Elixir migrations for ", appName, "..."]
  shelly $
    silently $ do
      setenv "MIX_ENV" "test"

      cd projectPath
      let (mix, ectoSetup) = mixEctoSetup
      run_ mix ectoSetup

mixEctoSetup :: (FilePath, [Text])
mixEctoSetup = let setupCommands = "do" : ["ecto.drop,", "ecto.create,", "ecto.migrate"] in ("mix", setupCommands)

elixirProjectPath :: Maybe Text -> Text -> FilePath
elixirProjectPath (Just home) projectName = fromText . T.intercalate "" $ [home, "/Development/zed/", projectName]
elixirProjectPath Nothing projectName = fromText projectName

main :: IO ()
main = do
  cArgs <- cmdArgs elixirSuite
  shelly $
    verbosely $
      do
        home <- get_env "HOME"

        when (withRedis cArgs) $ do
          void $ asyncSh runRedisServer

        when (isZedQL cArgs) $ do
          let (racing, zedApi) = ("racing-api", "zed-api")
          void $ runElixirMigrations (elixirProjectPath home zedApi) zedApi
          void $ runElixirMigrations (elixirProjectPath home racing) racing

        runMixSuite cArgs
