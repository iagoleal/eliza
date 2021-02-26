{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import qualified Data.Sequence   as S

import           Data.List       (unfoldr, find)
import           Data.Foldable   (toList)
import           Data.Bifunctor
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

getDecompRule = fst
getRecompRules = snd
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


-- Find Keywords
scanKeywords :: Script -> T.Text -> ([T.Text], [Keyword])
scanKeywords script input = let
  slices = parseReport phrasesParser "Error scanning keywords" input
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

-- Pattern match response
disassemble :: Parser [T.Text] -> T.Text -> Maybe [T.Text]
disassemble p input = parseMaybe p input

reassemble :: ReassemblyRule -> [T.Text] -> T.Text
reassemble rule ts = T.unwords $ fmap (assembler ts) rule
  where
   assembler _  (MatchText t) = t
   assembler ws (MatchN n)    = ws !! (n-1)
   assembler ws (MatchAll)    = T.unwords ws

keywordsMatcher :: Script -> [Keyword] -> T.Text -> T.Text
keywordsMatcher script [] input = pickAny (greetings script)
keywordsMatcher script (k:ks) input =
  case tryDecompRules (kwRules k) input of
    Nothing -> keywordsMatcher script ks input
    Just output -> output

tryDecompRules :: [Rule] -> T.Text -> Maybe T.Text
tryDecompRules [] input = Nothing
tryDecompRules (r:rs) input =
  case disassemble (getDecompRule r) input of
    Nothing -> tryDecompRules rs input
    Just ts -> let rRule = pickAny (getRecompRules r)
               in Just (reassemble rRule ts)

-- The bot per se
eliza :: Script -> T.Text -> T.Text
eliza script input =
  let (phrase, kwStack) = first T.unwords (scanKeywords script input)
  in keywordsMatcher script kwStack phrase

main :: IO ()
main = putStrLn "Hi"

-- TODO: be worked on
pickAny = head

-- Parser part

-- Chunk a phrase into phrases made of words
phrasesParser :: Parser [[T.Text]]
phrasesParser = phrase `sepEndBy` punctParser
 where phrase = space *> (wordParser `sepEndBy` space)
       wordParser = fmap T.pack (some validChar)

validChar :: Parser Char
validChar = satisfy (not . (\x -> isSeparator x || elem x puncts))
 where puncts = ".,!?;:" :: [Char]

punctParser :: Parser Char
punctParser = satisfy ((flip elem) (".,!?;:" :: [Char]))

-- Apply parser and report error in case of failure
parseReport p e t = maybe (error e) id (parseMaybe p t)

parserFromRule :: [MatchingRule] -> Parser [T.Text]
parserFromRule = sequence . unfoldr coalg
 where
  coalg :: [MatchingRule] -> Maybe (Parser T.Text, [MatchingRule])
  coalg [] = Nothing
  coalg [MatchAll] = Just (T.pack <$> manyTill (anySingle) eof, [])
  coalg (MatchAll:rest@(MatchText t:xs)) =
    Just (T.pack <$> manyTill anySingle (lookAhead $ string' t), rest)
  coalg (MatchText t:xs) = Just (string' t, xs)
