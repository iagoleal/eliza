{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad
import Control.Exception

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

import Text.Read (readMaybe)

import CLI
import Eliza

-- | The default configuration.
-- Used if the program receives no flag or argument.
defaultOptions :: Config
defaultOptions = Config { cfgTypingSpeed = 80
                        , cfgScript      = defaultScript
                        }

main :: IO ()
main = do
  args <- getArgs
  (cfg, errors)  <- parseArguments args
  unless (null errors)
    (print errors)
  cliRepl cfg

-- | Get a list of command line arguments and return a configuration and possible errors.
parseArguments :: [String] -> IO (Config, [String])
parseArguments args = do
  let (actions, nonOptions, errors) = getOpt Permute options args
  opts <- foldl (>>=) (pure defaultOptions) actions
  cfg <- case nonOptions of
    []        -> pure opts
    (fname:_) -> do
      script <- loadScriptOrExit fname
      pure $ opts {cfgScript = script}
  pure (cfg, errors)

-- | Like 'Eliza.loadScript' but exit the program
-- if the file does not exist or is not a proper JSON script.
loadScriptOrExit :: FilePath -> IO Script
loadScriptOrExit fname = catches (loadScript fname)
  [ Handler $ \ (e :: IOException) ->
      if isDoesNotExistError e
       then do
         prog <- getProgName
         hPutStrLn stderr (prog <> ": no such file or directory -- " <> show fname)
         exitWith (ExitFailure 1)
       else throw e
  , Handler $ \ (ScriptReadException _ msg :: ScriptReadException) -> do
     prog <- getProgName
     hPutStrLn stderr $ prog <> ": error parsing script file"
     hPutStrLn stderr msg
     exitWith (ExitFailure 1)
  ]

-- | Command line options to parse with 'System.Console.GetOpt'.
options :: [OptDescr (Config -> IO Config)]
options =
  [ Option [] ["wps", "words-per-second"]
    (ReqArg
      (\arg opt -> case (readMaybe arg :: Maybe Int) of
        Nothing -> do
          prog <- getProgName
          hPutStrLn stderr (prog <> ": invalid --wps argument \'" <> arg <> "\'\nShould be an integer.")
          exitWith (ExitFailure 1)
        Just n  -> pure opt{cfgTypingSpeed = n})
      "<num>")
    "How fast can Eliza type?"
  , Option "v" ["version"]
    (NoArg (\_ -> do
      prog <- getProgName
      hPutStrLn stderr (prog <> " version 0.1.0")
      exitWith ExitSuccess))
    "Print version and exit"
  , Option "h" ["help"]
    (NoArg (\_ -> do
      prog <- getProgName
      let header = "Usage: " <> prog <> " [OPTIONS]... [FILE]"
      hPutStrLn stderr (usageInfo header options)
      exitWith ExitSuccess))
    "Show this help message and exit"
  ]
