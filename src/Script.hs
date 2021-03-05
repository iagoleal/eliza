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
data Rule = Rule [MatchingRule] (V.Vector [ReassemblyRule])
  deriving Show

getDecompRule (Rule x _) = x
getRecompRules (Rule _ x) = x

-- The possible ways to match a text
data MatchingRule = MatchWord T.Text
                  | MatchAll
                  | MatchN Int
                  | MatchChoice [T.Text]
                  | MatchGroup T.Text
  deriving Show

data ReassemblyRule = ReturnText T.Text
                    | ReturnIndex Int
  deriving Show


loadScript :: FilePath -> IO Script
loadScript filename = do
  json <- LB.readFile filename
  pure $ case Aeson.eitherDecode json of
    Left  emsg   -> error $ "Failed to load file " <> filename
                          <> "\nError message: " <> emsg
    Right script -> script

findKeyword :: T.Text -> Script -> Maybe Keyword
findKeyword w script = M.lookup (T.toLower w) (keywords script)

findReflection :: T.Text -> Script -> Maybe T.Text
findReflection w script = M.lookup (T.toLower w) (reflections script)

{-
  Megaparsec Parsers
-}

readDecompRule :: T.Text -> [MatchingRule]
readDecompRule input = concat (parseMaybe parserMatchingRules input)

readReassemblyRule :: T.Text -> [ReassemblyRule]
readReassemblyRule input = concat (parseMaybe parserReassemblyRules input)

parserMatchingRules :: Parser [MatchingRule]
parserMatchingRules = space *> some rule
  where
   matchWord, matchAll, matchN, matchChoice, matchGroup :: Parser MatchingRule
   matchWord   = MatchWord   <$> word
   matchAll    = MatchAll    <$  symbol "*"
   matchN      = MatchN      <$> (char '#' *> positiveInteger)
   matchChoice = MatchChoice <$> brackets (some word)
   matchGroup  = MatchGroup  <$> (char '@' *> word)
   rule = choice [matchAll, matchN, matchChoice, matchGroup, matchWord]
   specialChars = "*#[]@" :: [Char]

parserReassemblyRules :: Parser [ReassemblyRule]
parserReassemblyRules = space *> some (returnIndex <|> returnText)
  where
   returnIndex, returnText :: Parser ReassemblyRule
   returnIndex = ReturnIndex <$> (char '$' *> L.decimal)
   returnText  = ReturnText . T.pack <$> some validChar
   validChar   = satisfy (not .  (=='$'))

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol' spaceConsumer

exactWord :: T.Text -> Parser T.Text
exactWord word = lexeme (string' word <* notFollowedBy alphaNumChar)

word :: Parser T.Text
word = lexeme $ T.pack <$> some (alphaNumChar <|> char '\'' <|> char '-')

brackets = between (symbol "[") (symbol "]")

positiveInteger :: Parser Int
positiveInteger = do
  d <- lexeme L.decimal
  if d == 0
   then fail "Zero index not allowed"
   else pure d

parserFromRule :: [MatchingRule] -> Parser [T.Text]
parserFromRule = sequence . unfoldr coalg
 where
  coalg :: [MatchingRule] -> Maybe (Parser T.Text, [MatchingRule])
  coalg [] = Nothing
  coalg (rule:rs) = passOn (f rule)
   where
    passOn x = Just (x, rs)
    f rule = case whatParser rule of
      Just p  -> p
      Nothing -> case listToMaybe rs of
        Nothing -> T.pack <$> manyTill (anySingle) eof
        Just x  -> case whatParser x of
          Just p  -> T.strip . T.pack <$> manyTill anySingle (lookAhead p)
          Nothing -> T.strip . T.pack <$> manyTill anySingle eof
  whatParser x = case x of
      MatchWord   w  -> Just (exactWord w)
      MatchChoice ws -> Just $ choice (fmap exactWord ws)
      MatchGroup  g  -> Nothing -- TODO implement group lookup with Script State
      MatchN      n  -> Just (T.unwords <$> count n word)
      MatchAll       -> Nothing

{-
  Read from JSON
-}
instance Aeson.FromJSON Script where
 parseJSON = Aeson.withObject "Script" $ \ v -> do
   greetings   <- v .: "greetings"
   defaultSays <- v .: "default"
   reflections <- v .: "reflections"
   keywords    <- v .: "keywords"
   pure $ Script { greetings   = greetings
                 , defaultSays = defaultSays
                 , reflections = reflections
                 , keywords    = M.fromList (fmap (kwWord &&& id) keywords)
                 }

instance Aeson.FromJSON Keyword where
 parseJSON = Aeson.withObject "Keyword" $ \ v -> do
   word  <- T.toLower <$> v .: "keyword"
   prec  <- v .: "precedence"
   rules <- v .: "rules"
   pure $ Keyword word prec rules

instance Aeson.FromJSON Rule where
 parseJSON = Aeson.withObject "Rule" $ \ o -> do
   decomp <- readDecompRule <$> o .: "decomposition"
   recompStrings <- o .: "reassembly"
   let recomps = readReassemblyRule <$> recompStrings
   pure (Rule decomp recomps)
