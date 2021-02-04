{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import qualified Data.Sequence   as S
import           Data.List       (find)
import           Data.Foldable   (toList)

type Token = T.Text

data DecompRule = Rule T.Text [T.Text]
  deriving Show

data Keyword = Keyword { kwToken :: Token
                       , kwPrecedence :: Int
                       , kwRules :: [DecompRule]
                       }
  deriving Show


reflections :: M.Map T.Text T.Text
reflections = M.fromList
  [("am", "are"),
   ("was", "were"),
   ("i", "you"),
   ("i'd", "you would"),
   ("i've", "you have"),
   ("i'll", "you will"),
   ("my", "your"),
   ("are", "am"),
   ("you’ve", "I have"),
   ("you’ll", "I will"),
   ("your", "my"),
   ("yours", "mine"),
   ("you", "me"),
   ("me", "you")]

keywords :: [Keyword]
keywords = [ Keyword "happy" 5 []
           , Keyword "me"    4 []
           , Keyword "sad"  10 []
           ]

tokenize :: T.Text -> [Token]
tokenize = T.words . T.toLower

reflect :: [Token] -> [Token]
reflect = fmap reflectToken
  where
   reflectToken word = case M.lookup word reflections of
     Nothing -> word
     Just x  -> x

scanKeywords :: [Token] -> [Keyword]
scanKeywords l = toList $ loop l S.Empty (-1)
 where
  loop []     stack _ = stack
  loop (w:ws) stack p =
    if w == "." || w == ","
     then if null stack
           then loop ws stack p
           else stack
     else case find ((==w) . kwToken) keywords of
       Nothing -> loop ws stack p
       Just kw -> if (kwPrecedence kw) > p
                   then loop ws (kw S.:<| stack) (kwPrecedence kw)
                   -- Should use a Seq here
                   else loop ws (stack S.:|> kw) p


match :: [Keyword] -> [Token] -> T.Text
match = undefined

eliza :: T.Text -> T.Text
eliza input =
  let tokens   = reflect (tokenize input)
      keywords = scanKeywords tokens
  in match keywords tokens


main :: IO ()
main = putStrLn "Hello, I'm Eliza, your new therapist. How are you feeling today?"
