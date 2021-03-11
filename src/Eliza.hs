{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
scanKeywords input = case parse phrasesParser "" input of
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

reflect :: [T.Text] -> State BotState [T.Text]
reflect ws = do
  script <- botScript <$> get
  let exchange w = maybe w id (findReflection w script)
  pure (fmap exchange ws)

disassemble :: [MatchingRule] -> T.Text -> MaybeT (State BotState) [T.Text]
disassemble rs input = do
    p <- lift $ parserFromRule rs
    liftMaybe $ parseMaybe p input

reassemble :: [ReassemblyRule] -> [T.Text] -> MaybeT (State BotState) T.Text
reassemble rs ts = pure $ T.concat (fmap (assembler ts) rs)
 where
  assembler _  (ReturnText  t) = t
  assembler ws (ReturnIndex n) = ws !! (n-1)

keywordsMatcher :: [Keyword] -> T.Text -> State BotState T.Text
keywordsMatcher kws input =
  let results = fmap (flip matchKeyword input) kws
      defaultResponse = pickAny =<< gets (defaultSays . botScript)
  in maybe defaultResponse pure =<< runMaybeT (asum results)

matchKeyword :: Keyword -> T.Text -> MaybeT (State BotState) T.Text
matchKeyword keyword input = let rules = kwRules keyword
                             in  tryDecompRules rules input

tryDecompRules :: Traversable t => t Rule -> T.Text -> MaybeT (State BotState) T.Text
tryDecompRules rules input = asum (fmap (flip tryDecompRule input) rules)

tryDecompRule :: Rule -> T.Text -> MaybeT (State BotState) T.Text
tryDecompRule rule input =
  case getDecompRule rule of
    DKeyword t -> tryOtherKeyword t
    DRule rs -> do
      result <- disassemble rs input
      recomp <- lift $ pickAny (getRecompRules rule)
      case recomp of
        RNewkey     -> mzero
        RKeyword t  -> tryOtherKeyword t
        RRule rrule -> reassemble rrule result
 where
  tryOtherKeyword t = do
     script <- botScript <$> get
     kw <- liftMaybe $ findKeyword t script
     matchKeyword kw input

parserFromRule :: [MatchingRule] -> State BotState (Parser [T.Text])
parserFromRule = fmap sequence . unfoldrM coalg
 where
  coalg [] = pure Nothing
  coalg (rule:rs) = fmap (\x -> Just (x,rs)) $ matchingRuleParser rule >>= \case
    Just p -> pure p
    Nothing -> case rs of
      [] -> pure (T.pack <$> manyTill (anySingle) eof)
      (x:_)  -> matchingRuleParser x >>= \case
         Just p  -> pure (T.strip . T.pack <$> manyTill anySingle (lookAhead p))
         Nothing -> pure (T.strip . T.pack <$> manyTill anySingle eof)

matchingRuleParser :: MatchingRule -> State BotState (Maybe (Parser T.Text))
matchingRuleParser x = runMaybeT $ case x of
  MatchWord   w  -> pure (exactWord w)
  MatchChoice ws -> pure $ choice (fmap exactWord ws)
  MatchGroup  g  -> do
    script <- botScript <$> get
    grps   <- liftMaybe $ findGroup g script
    pure $ choice (fmap exactWord grps)
  MatchN      n  -> pure (T.unwords <$> count n word)
  MatchAll       -> mzero

{-
  Parser part
-}

-- Chunk a phrase into phrases made of words
phrasesParser :: Parser [[T.Text]]
phrasesParser = sepEndBy phrase punctParser
 where phrase = space *> some (lexeme (T.pack <$> some validChar))

validChar :: Parser Char
validChar = satisfy (not . (\x -> isSeparator x || elem x puncts))
 where puncts = ".,!?;:" :: [Char]

punctParser :: Parser Char
punctParser = satisfy ((flip elem) (".,!?;:" :: [Char]))
