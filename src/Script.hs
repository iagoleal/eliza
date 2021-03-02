{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Script where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import qualified Data.Vector     as V

import Data.List (unfoldr)
import Data.Bifunctor

import           Data.Void       (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.:))


-- For use in Megaparsec parsers
type Parser = Parsec Void T.Text

-- All the information about how a ELIZA bot should talk
data Script = Script { reflections :: (M.Map T.Text T.Text)
                     , keywords    :: V.Vector Keyword
                     , defaultSays :: V.Vector T.Text
                     , greetings   :: V.Vector T.Text
                     }

-- Store all the necessary info for a given keyword
data Keyword = Keyword { kwWord       :: T.Text
                       , kwPrecedence :: Int
                       , kwRules      :: V.Vector Rule
                       }

-- maybe I should delete it and use only keywords?
data Rule = Rule (Parser [T.Text]) (V.Vector ReassemblyRule)
getDecompRule (Rule x _) = x
getRecompRules (Rule _ x) = x


type ReassemblyRule = [MatchingRule]

-- The possible ways to match a text
data MatchingRule = MatchAll
                  | MatchText T.Text
                  | MatchN Int
  deriving Show


-- Create a parser from a decomposition rule
parserFromRule :: T.Text -> Parser [T.Text]
parserFromRule = undefined

parserFromRule' :: [MatchingRule] -> Parser [T.Text]
parserFromRule' = sequence . unfoldr coalg
 where
  coalg :: [MatchingRule] -> Maybe (Parser T.Text, [MatchingRule])
  coalg [] = Nothing
  coalg [MatchAll] = Just (T.pack <$> manyTill (anySingle) eof, [])
  coalg (MatchAll:rest@(MatchText t:xs)) =
    Just (T.pack <$> manyTill anySingle (lookAhead $ string' t), rest)
  coalg (MatchText t:xs) = Just (string' t, xs)

-- -- Read from JSON
-- instance Aeson.FromJSON Keyword where
--  parseJSON = Aeson.withObject "Keyword" $ \ v -> do
--    word  <- v .: "keyword"
--    prec  <- v .: "precedence"
--    untractedRules <- v .: "rules"
--    rules <- fmap parseRule (Aeson.parseJSON untractedRules)
--    pure $ Keyword word prec rules

-- instance Aeson.FromJSON Script where
--  parseJSON = Aeson.withObject "Script" $ \ v -> do
--    greetings   <- v .: "greetings"
--    defaultSays <- v .: "default"
--    reflections <- v .: "reflections"
--    keywords    <- v .: "keywords"
--    pure $ Script { greetings   = greetings
--                  , defaultSays = defaultSays
--                  , reflections = reflections
--                  , keywords    = keywords
--                  }

-- parseRule = undefined
-- parseRule = Aeson.withObject "Rule" $ \v -> do
--   decompRule  <- parserFromRule <$> v .: "decomposition"
--   recompRules <- parseRecompRuleArray <$> v .: "reassembly"
--   pure (decompRule, recompRules)

-- parseRuleArray = Aeson.withArray "Array of Rules" $ \v -> do
--   mapM parseRule (V.toList v)

-- parseRecompRule = undefined

-- parseRecompRuleArray = Aeson.withArray "Array of reassembly rules" $
--   \v -> do
--   mapM parseRecompRule (V.toList v)
