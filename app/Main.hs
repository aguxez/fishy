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

default (Text)

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
mixEctoSetup = let setupCommands = "do" : ["ecto.drop,", "ecto.create,", "ecto.migrate"] in ("mix", setupCommands)

elixirProjectPath :: Maybe Text -> Text -> FilePath
elixirProjectPath (Just home) projectName = fromText . T.intercalate "" $ [home, "/Development/zed/", projectName]
elixirProjectPath Nothing projectName = fromText projectName

closeRedisServer :: Sh ()
closeRedisServer = do
  echo "Trying to kill Redis server...\n"
  shelly . silently $ do
    process <- run "pgrep" ["6379"]
    unless (process == "") $ do
      echo "Killing Redis server...\n"
      run_ "kill" ["-s", "TERM", T.filter (/= '\n') process]

main :: IO ()
main = do
  cArgs <- cmdArgs elixirSuite
  shelly . verbosely $ do
    home <- get_env "HOME"

    when (withRedis cArgs) $ do
      void $ asyncSh runRedisServer

    when (isZedQL cArgs) $ do
      let (racing, zedApi) = ("racing-api", "zed-api")
      void $ runElixirMigrations (elixirProjectPath home zedApi) zedApi
      void $ runElixirMigrations (elixirProjectPath home racing) racing

    -- we're capturing the exit code here so we can do cleanup tasks even
    -- if the test suite fails
    suiteExitCode <- runMixSuite cArgs

    -- The redis server is started async and stays in the background so
    -- we're explicitly shutting it down
    when (withRedis cArgs) closeRedisServer

    exit suiteExitCode
