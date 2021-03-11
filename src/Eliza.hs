{-# LANGUAGE OverloadedStrings #-}
module Eliza (
    module Eliza
  , module Script
  ) where

import qualified Data.Text       as T
import qualified Data.Sequence   as S
import qualified Data.Vector     as V

import           Data.Foldable (toList, asum)
import           System.Random
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import           Data.Char (isSeparator)

import Script
import Utils

{-
  State
-}
data BotState = BotState { botScript :: Script
                         , botSeed   :: StdGen
                         }
  deriving Show

initBotState :: Script -> IO BotState
initBotState script = do
  seed <- newStdGen
  pure (BotState script seed)

-- The bot per se

eliza :: BotState -> (T.Text -> T.Text)
eliza botState input = evalState (answer input) botState

answer :: T.Text -> State BotState T.Text
answer input = do
  (ws, kwStack) <- scanKeywords input
  phrase <- fmap T.unwords (reflect ws)
  keywordsMatcher kwStack phrase

pickAny :: V.Vector a -> State BotState a
pickAny v = do
  bot <- get
  let (idx, nseed) = randomR (0, V.length v - 1) (botSeed bot)
  put bot{botSeed = nseed}
  V.indexM v idx

-- Find Keywords
scanKeywords :: T.Text -> State BotState ([T.Text], [Keyword])
scanKeywords input = case MP.parse phrasesParser "" input of
    Left _  -> pure ([], [])
    Right slices -> foldrM analyzeKeywords ([], []) slices
 where
  analyzeKeywords phrase remainder = do
    kwords <- scanKwChunk phrase
    pure $ case kwords of
      []  -> remainder
      kws -> (phrase, kws)

scanKwChunk :: [T.Text] -> State BotState [Keyword]
scanKwChunk wrds = do
  script <- botScript <$> get
  let loop [] stack _ = stack
      loop (w:ws) stack p = case findKeyword w script of
        Nothing -> loop ws stack p
        Just kw -> if kwPrecedence kw > p
                    then loop ws (kw S.:<| stack) (kwPrecedence kw)
                    else loop ws (stack S.:|> kw) p
  pure (toList $ loop wrds S.Empty (-1))

-- Pattern match response
disassemble :: [MatchingRule] -> T.Text -> Maybe [T.Text]
disassemble rs input = let p = parserFromRule rs
                       in  MP.parseMaybe p input

reassemble :: [ReassemblyRule] -> [T.Text] -> T.Text
reassemble rule ts = T.concat . fmap (assembler ts) $ rule
  where
   assembler _  (ReturnText  t) = t
   assembler ws (ReturnIndex n) = ws !! (n-1)

keywordsMatcher :: [Keyword] -> T.Text -> State BotState T.Text
keywordsMatcher kws input = do
  deft <- pickAny =<< gets (defaultSays . botScript)
  foldrM matchedOrDefault deft kws
  where
   matchedOrDefault kw remainder = maybe (pure remainder) id
                                         (tryDecompRules (kwRules kw) input)

tryDecompRules :: Traversable t => t Rule -> T.Text -> Maybe (State BotState T.Text)
tryDecompRules rules input = do
  (rule, text) <- asum ruleResult
  Just $ do
    rRule <- pickAny (getRecompRules rule)
    pure (reassemble rRule text)
  where
   ruleResult  = fmap (sequence . (id &&& disassembler)) rules
   disassembler r = disassemble (getDecompRule r) input

reflect :: [T.Text] -> State BotState [T.Text]
reflect ws = do
  script <- botScript <$> get
  let exchange w = maybe w id (findReflection w script)
  pure (fmap exchange ws)

parserFromRule :: [MatchingRule] -> Parser [T.Text]
parserFromRule = sequence . unfoldr coalg
 where
  coalg :: [MatchingRule] -> Maybe (Parser T.Text, [MatchingRule])
  coalg [] = Nothing
  coalg (rule:rs) = passOn (f rule)
   where
    passOn x = Just (x, rs)
    f r = case whatParser r of
      Just p  -> p
      Nothing -> case listToMaybe rs of
        Nothing -> T.pack <$> MP.manyTill (MP.anySingle) MP.eof
        Just x  -> case whatParser x of
          Just p  -> T.strip . T.pack <$> MP.manyTill MP.anySingle (MP.lookAhead p)
          Nothing -> T.strip . T.pack <$> MP.manyTill MP.anySingle MP.eof
  whatParser x = case x of
      MatchWord   w  -> Just (exactWord w)
      MatchChoice ws -> Just $ MP.choice (fmap exactWord ws)
      MatchGroup  g  -> Nothing -- TODO implement group lookup with Script State
      MatchN      n  -> Just (T.unwords <$> MP.count n word)
      MatchAll       -> Nothing


{-
  Parser part
-}

-- Chunk a phrase into phrases made of words
phrasesParser :: Parser [[T.Text]]
phrasesParser = MP.sepEndBy phrase punctParser
 where phrase = MP.space *> MP.some (lexeme (T.pack <$> MP.some validChar))

validChar :: Parser Char
validChar = MP.satisfy (not . (\x -> isSeparator x || elem x puncts))
 where puncts = ".,!?;:" :: [Char]

punctParser :: Parser Char
punctParser = MP.satisfy ((flip elem) (".,!?;:" :: [Char]))

-- Apply parser and report error in case of failure
parseReport p e t = maybe (error e) id (MP.parseMaybe p t)
