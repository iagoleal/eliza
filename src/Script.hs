{-# LANGUAGE OverloadedStrings #-}
module Script where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import Data.List (unfoldr)
import Data.Bifunctor

import           Data.Void       (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Data.Aeson

-- All the information about how a ELIZA bot should talk
data Script = Script { reflections :: (M.Map T.Text T.Text)
                     , keywords    :: [Keyword]
                     , defaultSays :: [T.Text]
                     , greetings   :: [T.Text]
                     }

data MatchingRule = MatchAll
                  | MatchText T.Text
                  | MatchN Int
  deriving Show

type Parser = Parsec Void T.Text
type Rule = (Parser [T.Text], [ReassemblyRule])

type ReassemblyRule = [MatchingRule]
data Keyword = Keyword { kwWord :: T.Text
                       , kwPrecedence :: Int
                       , kwRules :: [Rule]
                       }

getDecompRule :: Rule -> Parser [T.Text]
getDecompRule = fst

getRecompRules :: Rule -> [ReassemblyRule]
getRecompRules = snd

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

-- Read from JSON
instance FromJSON Keyword where
 parseJSON = withObject "Keyword" $ \ v -> do
   word  <- v .: "keyword"
   prec  <- v .: "precedence"
   rules <- v .: "rules"
   pure $ Keyword word prec rules


instance FromJSON Script where
 parseJSON = withObject "Script" $ \ v -> do
   greetings   <- v .: "greetings"
   defaultSays <- v .: "default"
   reflections <- v .: "reflections"
   keywords    <- v .: "keywords"
   pure $ Script { greetings   = greetings
                 , defaultSays = defaultSays
                 , reflections = reflections
                 , keywords    = keywords
                 }
