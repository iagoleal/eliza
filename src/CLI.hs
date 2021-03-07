module CLI (cliRepl) where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import Control.Monad.State
import Control.Monad

import Eliza

data UserInput = CmdQuit | CmdError | Input T.Text

cliRepl :: IO ()
cliRepl = do
  bot <- initBotState defaultScript
  let (greet, bot') = runState (pickAny (greetings . botScript $ bot)) bot
  cliOutput greet
  repl bot

repl bot = do
  input <- cliInput
  case processInput input of
    CmdQuit  ->
      cliOutput "Goodbye"
    CmdError ->
      cmdErrorMsg input >> repl bot
    Input t  -> do
      let (response, s) = runState (answer input) bot
      cliOutput response
      repl s

botPrompt :: T.Text
botPrompt = "eliza> "

userPrompt :: T.Text
userPrompt = "> "

cliInput = do
  T.putStr userPrompt
  input <- T.getLine
  T.putStrLn ""
  pure input

cliOutput out = T.putStrLn (botPrompt <> out <> "\n")

processInput input
  | elem input [":quit", ":q", ":bye"] = CmdQuit
  | not (T.null input) && T.head input == ':' = CmdError
  | otherwise = Input input

cmdErrorMsg input =
  T.putStrLn $ "Sorry, command non-recognized: " <> input
