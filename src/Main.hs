{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import qualified Data.Sequence   as S
import qualified Data.Map        as M
import qualified Data.Vector     as V

import           Data.List       (unfoldr, find)
import           Data.Foldable   (toList, asum)
import           Control.Arrow
import           Data.Void       (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Char (isSeparator)

import Script

-- To be substituted by a real script
myScript :: Script
myScript = Script
  ([("am", "are"),
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

reassemble :: [MatchingRule] -> [T.Text] -> T.Text
reassemble rule ts = T.unwords . fmap (assembler ts) $ rule
  where
   assembler _  (MatchText t) = t
   assembler ws (MatchN n)    = ws !! (n-1)
   assembler ws (MatchAll)    = T.unwords ws

keywordsMatcher :: Script -> [Keyword] -> T.Text -> T.Text
keywordsMatcher script kws input =
    foldr matchedOrDefault (pickAny $ defaultSays script) kws
  where
   matchedOrDefault kw def = maybe def id (tryDecompRules (kwRules kw) input)

tryDecompRules :: Traversable t => t Rule -> T.Text -> Maybe T.Text
tryDecompRules rules input =
  asum ruleResult >>= \(rule, text) ->
    let rRule = pickAny (getRecompRules rule)
    in pure (reassemble rRule text)
  where
   ruleResult = fmap (sequence . (id &&& disassembler)) rules
   disassembler r = disassemble (getDecompRule r) input

reflect :: Script -> [T.Text] -> [T.Text]
reflect script = fmap exchange
  where
   exchange w = maybe w id (M.lookup (T.toLower w) (reflections script))

-- The bot per se
eliza :: Script -> (T.Text -> T.Text)
eliza script input =
  let (words, kwStack) = (scanKeywords script input)
      phrase = T.unwords (reflect script words)
  in keywordsMatcher script kwStack phrase

main :: IO ()
main = do
  input <- TIO.getLine
  TIO.putStrLn (eliza myScript input)
  main

-- TODO: be worked on
pickAny = V.head

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
