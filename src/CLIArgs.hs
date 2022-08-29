{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module CLIArgs (Fishy (..), elixirSuite, weather) where

import Data.Text (Text)
import System.Console.CmdArgs.Implicit

data Fishy
  = Mix {project :: FilePath, testPath :: Maybe Text, redis :: Bool, zedQL :: Bool}
  | Weather {cityName :: Text}
  deriving (Show, Data, Typeable)

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
