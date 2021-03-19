module CLI where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           System.IO
import           System.Console.ANSI

import Control.Monad.State
import Control.Concurrent (threadDelay)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Eliza
import Utils

data UserInput = CmdQuit
               | CmdError
               | CmdHelp
               | CmdLoad FilePath
               | Input   T.Text
  deriving Show

cliRepl :: Script -> IO ()
cliRepl script = do
  initialMsg
  bot <- initBotState script
  flip evalStateT bot $ do
    greet  <- hoistState pickGreeting
    lift (cliOutput greet)
    repl

repl :: StateT BotState IO ()
repl = do
  input <- lift cliInput
  case processInput input of
    CmdQuit  -> hoistState pickGoodbye >>= cliOutput
    CmdError -> cmdErrorMsg input >> repl
    CmdHelp  -> helpMsg           >> repl
    CmdLoad file -> do
      script <- lift $ loadScript file
      modify (\bot -> bot{botScript = script})
      repl
    Input t  -> do
      response <- hoistState (answer t)
      disappearingPrint (typingTime averageWPS response) "Eliza is typing..."
      cliOutput response
      repl

averageWPS :: Int
averageWPS = 80

typingTime :: Int -> T.Text -> Int
typingTime wps text = 10^6 * T.length text `quot` wps

disappearingPrint :: MonadIO m => Int -> T.Text -> m ()
disappearingPrint time phrase = liftIO $ do
  T.putStr phrase
  hFlush stdout
  threadDelay time
  clearLine
  setCursorColumn 0

initialMsg :: MonadIO m => m ()
initialMsg = liftIO $ do
  T.putStr "Welcome to "
  yellow   "ELIZA"
  T.putStr "!\n"
  T.putStr "Type your questions and press 'Enter'\n"
  T.putStr "For more info, enter '"
  cyan ":help"
  T.putStr "'\n\n"

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

cliInput :: MonadIO m => m T.Text
cliInput = liftIO $ do
  green ">>>>>> "
  hFlush stdout
  input <- T.getLine
  T.putStrLn ""
  pure input

cliOutput :: MonadIO m => T.Text -> m ()
cliOutput out = liftIO $ yellow "eliza> " >> T.putStrLn (out <> "\n")

yellow :: MonadIO m => T.Text -> m ()
yellow = putStrAnsi [SetColor Foreground Vivid Yellow]

green :: MonadIO m => T.Text -> m ()
green = putStrAnsi [ SetColor Foreground Vivid Green
                   , SetConsoleIntensity BoldIntensity ]

cyan :: MonadIO m => T.Text -> m ()
cyan = putStrAnsi [SetColor Foreground Dull Cyan]

putStrAnsi :: MonadIO m => [SGR] -> T.Text -> m ()
putStrAnsi l s = liftIO $ setSGR l >> T.putStr s >> setSGR [Reset]

cmdErrorMsg :: MonadIO m => T.Text -> m ()
cmdErrorMsg input = liftIO $ do
  T.putStr "Sorry, non-recognized command: "
  cyan (input <> "\n")


processInput :: T.Text -> UserInput
processInput s = maybe (Input s) id $ MP.parseMaybe commands s
  where
   cmdQuit  = CmdQuit  <$  (parseCmd ["quit", "q", "bye"] <* MP.many MP.anySingle)
   cmdHelp  = CmdHelp  <$  (parseCmd ["help", "h"] <* MP.many MP.anySingle)
   cmdLoad  = CmdLoad  <$> (parseCmd ["load", "l"] *> MP.some MP.anySingle)
   cmdError = CmdError <$ (MP.space *> MP.char ':' *> MP.many MP.anySingle)
   commands = MP.choice $ fmap MP.try [cmdHelp, cmdQuit, cmdLoad, cmdError]

parseCmd :: (Foldable f, Functor f) => f T.Text -> Parser T.Text
parseCmd xs = MP.space *> MP.char ':' *> MP.choice (fmap exactWord xs)

