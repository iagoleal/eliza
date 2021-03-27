{-|
Module      : CLI
Description : Command Line Interface for eliza
Copyright   : (c) Iago Leal de Freitas, 2021
License     : GPL-3
Maintainer  : hello@iagoleal.com
Stability   : experimental
Portability : POSIX, Windows

Define the necessary methods to run a bot in a CLI.
Everything in here should be ANSI compatible and work
in both POSIX compliant terminals and on the Windows cmd / powershell.
-}
module CLI
  ( cliRepl
  , Config(..)
  ) where

import           Control.Monad.State
import           Control.Concurrent (threadDelay)
import           System.IO
import           System.Console.ANSI

import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Eliza
import Utils

-- | The configuration necessary to run a Eliza program.
data Config = Config
  { cfgTypingSpeed :: Int     -- ^ On average, how many words the bot types per second.
  , cfgScript      :: Script  -- ^ Where the script file is located.
  }


-- | Which kind of input did the user enter.
data UserInput = CmdQuit            -- ^ User asked to close program
               | CmdHelp            -- ^ User asked to see help message
               | CmdError T.Text    -- ^ Input started with @:@ but is not a recognized command
               | CmdLoad  FilePath  -- ^ User asked to load script from a file
               | Input    T.Text    -- ^ Normal user input
  deriving Show

-- * The Command Line Interface

{- | Start an eliza REPL on the command line.

  Accepts both real inputs and commands prefixed with a @:@.

  This function is supposed to run indefinitely
  or until the user explicitly quits the program.
-}
cliRepl :: Config -> IO ()
cliRepl cfg = do
  initialMsg
  bot <- initBotState (cfgScript cfg)
  flip evalStateT bot $ do
    greet <- hoistState pickGreeting
    liftIO (cliOutput greet)
    makeRepl cfg

-- | Given a configuration, generate a REPL.
makeRepl :: Config -> StateT BotState IO ()
makeRepl cfg = repl
 where
  repl = do
    input <- readUserInput
    evalAndPrint cfg input
    unless (isExitCommand input)
      repl

-- | Read user input from stdin.
readUserInput :: MonadIO m => m UserInput
readUserInput = cliInput >>= pure . processInput

-- | Evaluate the user input and print the proper output.
-- If it is a command, execute it.
-- Else, the bot is queried for an answer and the result printed to stdout.
evalAndPrint :: Config -> UserInput -> StateT BotState IO ()
evalAndPrint Config {cfgTypingSpeed = wps} = \case
  CmdQuit    -> hoistState pickGoodbye >>= cliOutput
  CmdError e -> cmdErrorMsg e
  CmdHelp    -> helpMsg
  CmdLoad file -> do
    script <- liftIO (loadScript file)
    modify (\bot -> bot{botScript = script})
  Input t  -> do
    response <- hoistState (answer t)
    disappearingPrint (typingTime wps response) "Eliza is typing..."
    cliOutput response

-- | Test whether the user wants to leave the program.
isExitCommand :: UserInput -> Bool
isExitCommand CmdQuit = True
isExitCommand _       = False

-- | Given an average of words per second, calculate the time to type a given text.
typingTime :: Int -> T.Text -> Int
typingTime wps text = 10^6 * T.length text `quot` wps

-- | Get user input form stdin with a prompt
cliInput :: MonadIO m => m T.Text
cliInput = liftIO $ do
  green ">>>>>> "
  hFlush stdout
  input <- T.getLine
  T.putStrLn ""
  pure input

-- | Print to stdout with a prompt (to use on bot answers)
cliOutput :: MonadIO m => T.Text -> m ()
cliOutput out = liftIO $ yellow "eliza> " >> T.putStrLn (out <> "\n")

-- * Messages

-- | The CLI header message
initialMsg :: MonadIO m => m ()
initialMsg = liftIO $ do
  T.putStr "Welcome to "
  yellow   "ELIZA"
  T.putStr "!\n"
  T.putStr "Type your questions and press 'Enter'\n"
  T.putStr "For more info, enter '"
  cyan ":help"
  T.putStr "'\n\n"

-- | What to print on screen when the user asks for help
helpMsg :: MonadIO m => m ()
helpMsg = liftIO $ do
  T.putStrLn "Available commands:\n"
  cyan "  :help"
  T.putStrLn "\t    show this message"
  cyan "  :load"
  T.putStrLn "\t    load new script"
  cyan "  :quit"
  T.putStrLn "\t    exit Eliza"
  T.putStrLn ""

-- | Message for wrong command
cmdErrorMsg :: MonadIO m => T.Text -> m ()
cmdErrorMsg input = liftIO $ do
  T.putStr "Sorry, non-recognized command: '"
  cyan (":" <> input)
  T.putStr "'\n"

-- * Parse commands

-- | Process the input text,
-- checking if it is a normal input or a command string.
-- In the later case, also process it to see which command it is.
processInput :: T.Text -> UserInput
processInput s = maybe (Input s) id $ MP.parseMaybe commands s
  where
   commands = MP.choice . fmap MP.try $
     [ CmdQuit  <$  (parseCmd ["quit", "q", "bye"] <* MP.many MP.anySingle)
     , CmdHelp  <$  (parseCmd ["help", "h", "?"] <* MP.many MP.anySingle)
     , CmdLoad  <$> (parseCmd ["load", "l"] *> MP.some MP.anySingle)
     , CmdError . T.pack <$> (MP.space *> MP.char ':' *> MP.many MP.anySingle)
     ]

-- | Helper function to parse a command
-- that can start with multiple keywords
parseCmd :: (Foldable f, Functor f) => f T.Text -> Parser T.Text
parseCmd xs = MP.space *> MP.char ':' *> MP.choice (fmap exactWord xs)

-- * ANSI terminal helpers

-- | Print a text and make it disappear.
disappearingPrint :: MonadIO m => Int -> T.Text -> m ()
disappearingPrint time phrase = liftIO $ do
  T.putStr phrase
  hFlush stdout
  threadDelay time
  clearLine
  setCursorColumn 0

yellow :: MonadIO m => T.Text -> m ()
yellow = putStrAnsi [SetColor Foreground Vivid Yellow]

green :: MonadIO m => T.Text -> m ()
green = putStrAnsi [ SetColor Foreground Vivid Green
                   , SetConsoleIntensity BoldIntensity ]

cyan :: MonadIO m => T.Text -> m ()
cyan = putStrAnsi [SetColor Foreground Dull Cyan]

putStrAnsi :: MonadIO m => [SGR] -> T.Text -> m ()
putStrAnsi l s = liftIO $ setSGR l >> T.putStr s >> setSGR [Reset]
