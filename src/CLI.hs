module CLI where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import Control.Monad.State
import System.IO

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Eliza

data UserInput = CmdQuit | CmdError | CmdHelp | CmdLoad FilePath | Input T.Text
  deriving Show

hoistState :: State s a -> StateT s IO a
hoistState = state . runState

cliRepl :: Script -> IO ()
cliRepl script = do
    T.putStrLn initialMsg
    bot <- initBotState script
    flip evalStateT bot $ do
      greets <- greetings . botScript <$> get
      greet  <- hoistState (pickAny greets)
      lift (cliOutput greet)
      repl

repl :: StateT BotState IO ()
repl = do
  input <- lift cliInput
  case processInput input of
    CmdQuit  -> lift (cliOutput "Goodbye")
    CmdError -> lift (cmdErrorMsg input) >> repl
    CmdHelp  -> lift (T.putStrLn helpMsg) >> repl
    CmdLoad file -> do
      script <- lift $ loadScript file
      modify (\bot -> bot{botScript = script})
      repl
    Input t  -> do
      response <- hoistState (answer t)
      lift (cliOutput response)
      repl

botPrompt :: T.Text
botPrompt = "eliza> "

userPrompt :: T.Text
userPrompt = "> "

initialMsg :: T.Text
initialMsg =
  "Welcome to ELIZA!\n\
  \Type your questions and press 'Enter'\n\
  \For more info, type ':help'\n\n"

helpMsg :: T.Text
helpMsg =
  "Available commands:\n\n\
  \  :help\t    show this message\n\
  \  :load\t    load new script\n\
  \  :quit\t    exit Eliza\n\
  \\n"

cliInput :: IO T.Text
cliInput = do
  T.putStr userPrompt
  hFlush stdout
  input <- T.getLine
  T.putStrLn ""
  pure input

cliOutput :: T.Text -> IO ()
cliOutput out = T.putStrLn (botPrompt <> out <> "\n")

processInput :: T.Text -> UserInput
processInput s = maybe (Input s) id $ MP.parseMaybe commands s
  where
   cmdQuit  = CmdQuit  <$  parseCmd ["quit", "q", "bye"]
   cmdHelp  = CmdHelp  <$  parseCmd ["help", "h"]
   cmdLoad  = CmdLoad  <$> (parseCmd ["load", "l"] >> MP.some MP.anySingle)
   cmdError = CmdError <$ (MP.char ':' >> MP.many MP.anySingle)
   commands = MP.choice $ fmap MP.try [cmdHelp, cmdQuit, cmdLoad, cmdError]

parseCmd :: (Foldable f, Functor f) => f T.Text -> Parser T.Text
parseCmd xs = MP.char ':' *> (MP.choice $ fmap exactWord xs)

safeTextHead :: T.Text -> Maybe Char
safeTextHead = fmap fst . T.uncons

cmdErrorMsg :: T.Text -> IO ()
cmdErrorMsg input = T.putStrLn $ "Sorry, non-recognized command: " <> input
