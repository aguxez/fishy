{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Data.Text as T
import Shelly
import System.Environment (getArgs)

default (T.Text)

runRedisServer :: Sh ()
runRedisServer = do
  echo "Running Redis Server..."
  shelly $
    silently $ do
      run_ "redis-server" []

runMixSuite :: FilePath -> Sh ()
runMixSuite elixirProject = do
  echo "Running mix test suite..."
  shelly $
    verbosely $ do
      cd elixirProject
      run_ "mix" ["test"]

main :: IO ()
main = do
  shelly $
    verbosely $ do
      args <- liftIO getArgs
      case args of
        ["redis-full", elixirPath] -> do
          _ <- asyncSh runRedisServer
          runMixSuite elixirPath
        _ -> fail "no match"
