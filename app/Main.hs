module Main where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import           System.Environment

import CLI
import Eliza

main :: IO ()
main = do
  args <- getArgs
  script <- case args of
    [] -> pure defaultScript
    (x:xs) -> loadScript x
  cliRepl script
