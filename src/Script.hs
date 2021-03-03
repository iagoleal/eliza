{-# LANGUAGE OverloadedStrings #-}
module Script where

import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

import qualified Data.Text       as T
import qualified Data.ByteString.Lazy as LB

import Data.List (unfoldr)
import Data.Maybe
import Control.Arrow ((&&&))

import           Data.Void       (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:))


-- For use in Megaparsec parsers
type Parser = Parsec Void T.Text

-- All the information about how a ELIZA bot should talk
data Script = Script { reflections :: M.Map T.Text T.Text
                     , keywords    :: M.Map T.Text Keyword
                     , defaultSays :: V.Vector T.Text
                     , greetings   :: V.Vector T.Text
                     }
  deriving Show

-- Store all the necessary info for a given keyword
data Keyword = Keyword { kwWord       :: T.Text
                       , kwPrecedence :: Int
                       , kwRules      :: V.Vector Rule
                       }
  deriving Show

-- maybe I should delete it and use only keywords?
data Rule = Rule (Parser [T.Text]) (V.Vector [MatchingRule])

instance Show Rule where
 show (Rule a b) = "(Rule Parser " ++ show b

getDecompRule (Rule x _) = x
getRecompRules (Rule _ x) = x

-- The possible ways to match a text
data MatchingRule = MatchAll
                  | MatchText T.Text
                  | MatchN Int
  deriving Show

loadScript :: T.Text -> IO Script
loadScript filename = do
  json <- LB.readFile (T.unpack filename)
  pure $ maybe errormsg id (Aeson.decode json)
 where errormsg = error $ "Failed to load file " ++ T.unpack filename

{-
  Megaparsec Parsers
-}

readDecompRule :: T.Text -> Parser [T.Text]
readDecompRule = parserFromRule . readRecompRule

readRecompRule :: T.Text -> [MatchingRule]
readRecompRule input = concat (parseMaybe parserMatchingRules input)

parserMatchingRules :: Parser [MatchingRule]
parserMatchingRules = some matchingRule
  where
   matchAll, matchN, matchText :: Parser MatchingRule
   matchAll  = (MatchAll <$ char '*')
   matchN    = MatchN <$> (char '$' *> L.decimal)
   matchText = MatchText . T.pack <$> (space *> (some validChar <|> some spaceChar))
   matchingRule = choice [matchAll, matchN, matchText]
   validChar = satisfy (not . \x -> elem x ['$', '*'])

-- Create a parser from a decomposition rule
parserFromRule :: [MatchingRule] -> Parser [T.Text]
parserFromRule = sequence . unfoldr coalg
 where
  coalg :: [MatchingRule] -> Maybe (Parser T.Text, [MatchingRule])
  coalg [] = Nothing
  coalg [MatchAll] = Just (T.pack <$> manyTill (anySingle) eof, [])
  coalg (MatchAll:rest@(MatchText t:xs)) =
    Just (T.pack <$> manyTill anySingle (lookAhead $ string' t), rest)
  coalg (MatchText t:xs) = Just (string' t, xs)

{-
  Read from JSON
-}
instance Aeson.FromJSON Script where
 parseJSON = Aeson.withObject "Script" $ \ v -> do
   greetings   <- v .: "greetings"
   defaultSays <- v .: "default"
   reflections <- v .: "reflections"
   keywordList <- v .: "keywords"
   let keywords = fmap (kwWord &&& id) keywordList
   pure $ Script { greetings   = greetings
                 , defaultSays = defaultSays
                 , reflections = reflections
                 , keywords    = M.fromList keywords
                 }

instance Aeson.FromJSON Keyword where
 parseJSON = Aeson.withObject "Keyword" $ \ v -> do
   word  <- v .: "keyword"
   prec  <- v .: "precedence"
   rules <- v .: "rules"
   pure $ Keyword word prec rules

instance Aeson.FromJSON Rule where
 parseJSON = Aeson.withObject "Rule" $ \ o -> do
   decomp <- readDecompRule <$> o .: "decomposition"
   recompStrings <- o .: "reassembly"
   let recomps = readRecompRule <$> recompStrings
   pure (Rule decomp recomps)
