module Main where

import           System.Environment

import CLI
import Eliza

main :: IO ()
main = do
  args <- getArgs
  script <- case args of
    [] -> pure defaultScript
    (x:_) -> loadScript x
  cliRepl script
