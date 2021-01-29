{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Map.Strict  as M

reflections :: M.Map T.Text T.Text
reflections = M.fromList
  [("am", "are"),
  ("was", "were"),
  ("i", "you"),
  ("i’d", "you would"),
  ("i’ve", "you have"),
  ("i’ll", "you will"),
  ("my", "your"),
  ("are", "am"),
  ("you’ve", "I have"),
  ("you’ll", "I will"),
  ("your", "my"),
  ("yours", "mine"),
  ("you", "me"),
  ("me", "you")]

tokenize :: T.Text -> [T.Text]
tokenize = T.words

reflect :: [T.Text] -> [T.Text]
reflect = fmap reflectToken
  where
   reflectToken word = case M.lookup word reflections of
     Nothing -> word
     Just x  -> x

scanKeywords :: [T.Text] -> [T.Text]
scanKeywords = undefined

match :: [T.Text] -> [T.Text] -> T.Text
match = undefined

eliza :: T.Text -> T.Text
eliza input =
  let tokens   = reflect (tokenize input)
      keywords = scanKeywords tokens
  in match keywords tokens


main :: IO ()
main = putStrLn "Hello, I'm Eliza, your new therapist. How are you feeling today?"
