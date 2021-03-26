module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import CLI
import Eliza

-- | The configuration necessary to run a Eliza program
data Config = Config
  { cfgTypingSpeed :: Int     -- ^ On average, how many words the bot types per second.
  , cfgScript      :: Script  -- ^ Where the script file is located.
  }

-- | The default configuration.
-- Used if the program receives no flag or argument.
defaultOptions :: Config
defaultOptions = Config { cfgTypingSpeed = 80
                        , cfgScript = defaultScript
                        }

main :: IO ()
main = do
  args <- getArgs
  (cfg, errors)  <- parseArguments args
  print $ errors
  cliRepl (cfgScript cfg)

-- | Get a list of command line arguments and return a configuration and possible errors.
parseArguments :: [String] -> IO (Config, [String])
parseArguments args = do
  let (actions, nonOptions, errors) = getOpt Permute options args
  opts <- foldl (>>=) (pure defaultOptions) actions
  cfg <- case nonOptions of
    []        -> pure opts
    (fname:_) -> do
      script <- loadScript fname
      pure $ opts {cfgScript = script}
  pure (cfg, errors)

-- | Command line options to parse with 'System.Console.GetOpt'.
options :: [OptDescr (Config -> IO Config)]
options =
  [ Option [] ["wps", "words-per-second"]
    (ReqArg
      (\arg opt -> pure opt {cfgTypingSpeed = read arg})
      "<num>")
    "How fast can Eliza type?"
  , Option "v" ["version"]
    (NoArg (\_ -> do
      prog <- getProgName
      hPutStrLn stderr (prog <> " version 0.1.0")
      exitWith ExitSuccess)
    )
    "Print version and exit"
  , Option "h" ["help"]
    (NoArg (\_ -> do
      prog <- getProgName
      let header = "Usage: " <>  prog <> " [OPTIONS]... [FILE]"
      hPutStrLn stderr (usageInfo header options)
      exitWith ExitSuccess)
    )
    "Show this help message and exit"
  ]
