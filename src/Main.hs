{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text       as T
import           Data.Text (Text)

import qualified Data.Map.Strict as M
import qualified Data.Sequence   as S

import           Data.List       (find)
import           Data.Foldable   (toList)
import           Data.Void       (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Char (isSeparator)

type Parser = Parsec Void T.Text

data MatchingRule = MatchAll
                  | MatchText T.Text
                  | MatchN Int
  deriving Show

type Rule = (Parser [T.Text], [ReassemblyRule])
type ReassemblyRule = [MatchingRule]
data Keyword = Keyword { kwWord :: T.Text
                       , kwPrecedence :: Int
                       , kwRules :: [Rule]
                       }

data Script = Script { reflections :: (M.Map T.Text T.Text)
                     , keywords    :: [Keyword]
                     , defaultSays :: [T.Text]
                     , greetings   :: [T.Text]
                     }

myScript :: Script
myScript = Script
  (M.fromList
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
   ("me", "you")])
   [Keyword "happy" 5 []
   , Keyword "me"   4 []
   , Keyword "sad" 10 []
   ]
   ["How do you feel about that?"]
   ["Hello, I'm Eliza, your new therapist. How are you feeling today?"]

reflect script input =
  let wds = words input
  in undefined

scanKeywords :: Script -> T.Text -> ([T.Text], [Keyword])
scanKeywords script input = let
  slices = case parseMaybe phrasesParser input of
    Nothing -> error "Error parsing input"
    Just x  -> x
  in foldr (analyzeKeywords script) ([], []) slices
  where
   analyzeKeywords script phrase remainder =
     case scanKwChunk script phrase of
      []  -> remainder
      kws -> (phrase, kws)

scanKwChunk :: Script -> [T.Text] -> [Keyword]
scanKwChunk script ws = toList $ loop ws S.Empty (-1)
  where
   loop [] stack _ = stack
   loop (w:ws) stack p =
     case find ((==w) . kwWord) (keywords script) of
       Nothing -> loop ws stack p
       Just kw -> if kwPrecedence kw > p
                   then loop ws (kw S.:<| stack) (kwPrecedence kw)
                   else loop ws (stack S.:|> kw) p

eliza :: Script -> T.Text -> T.Text
eliza script input =
  let (phrase, kwStack) = scanKeywords script input
  in if null kwStack
      then head (greetings script)
      else undefined

main :: IO ()
main = putStrLn "Hi"

-- Parser part

-- Chunk a phrase into phrases made of words
phrasesParser :: Parser [[Text]]
phrasesParser = phrase `sepEndBy` punctParser

phrase :: Parser [Text]
phrase = space *> (wordParser `sepEndBy` space)

wordParser = fmap T.pack (some validChar)

validChar :: Parser Char
validChar = satisfy (not . (\x -> isSeparator x || elem x puncts))
 where puncts = ".,!?;:" :: [Char]

punctParser :: Parser Char
punctParser = satisfy (\x -> elem x (".,!?;:" :: [Char]))
