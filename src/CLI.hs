module CLI where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import Control.Monad.State

import Eliza

cliRepl :: IO ()
cliRepl = do
  bot <- initBotState =<< loadScript "scripts/doctor.json"
  go bot
 where
  go bot = do
    input <- T.getLine
    let (r, s) = runState (answer input) bot
    T.putStrLn r
    go s
