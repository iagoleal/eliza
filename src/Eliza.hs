{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Eliza (
    module Eliza
  , module Script
  ) where

import qualified Data.Text       as T
import qualified Data.Sequence   as S
import qualified Data.Vector     as V
import           Data.Sequence (Seq(..))

import           Data.Foldable (toList, asum)
import           System.Random
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import           Data.Char (isSeparator)

import Script
import Utils

-- * Bot configuration
data BotState = BotState { botScript     :: Script
                         , botSeed       :: StdGen
                         , botMemory     :: S.Seq T.Text
                         , botMemoryProb :: Double
                         }
  deriving Show

initBotState :: Script -> IO BotState
initBotState script = do
  seed <- newStdGen
  pure (BotState script seed S.empty 0.4)

-- ** The bot per se

{- | Bot main procedure.
  Receive some input and answer accordingly to the state of the bot.

  The process follows these steps:

  - Scan input looking for keywords
  - Keep only the first part of input (between punctuations) where we find any keyword
  - Apply reflections to each (possible) word according to script
  - If we find no keyword, answer with a phrase from memory or pick a default answer
  - Else, try to commit the phrase to memory
    and try to match the keywords to it.
  - Answer accordingly or with a default answer.
-}
answer :: T.Text -> State BotState T.Text
answer input = do
  (ws, kwStack) <- scanKeywords input
  phrase <- T.unwords <$> reflect ws
  case kwStack of
    [] -> maybeRemember >>= maybe pickDefaultSay pure
    xs -> do
      commitToMemory (head xs) phrase
      keywordsMatcher xs phrase


-- ** Memory Related
maybeRemember :: State BotState (Maybe T.Text)
maybeRemember = do
  p <- pickRandom
  prob <- gets botMemoryProb
  memo <- gets botMemory
  if p > prob
   then case memo of
     S.Empty -> pure Nothing
     h :<| t -> modify (\bot -> bot{botMemory = t}) >> pure (Just h)
   else pure Nothing

commitToMemory :: Keyword -> T.Text -> State BotState ()
commitToMemory kw input =
  let memo = kwMemory kw
  in runMaybeT (tryDecompRules memo input) >>= maybe (pure ()) memorize

memorize :: T.Text -> State BotState ()
memorize t = do
  bot <- get
  let memo = botMemory bot
  put bot{botMemory = memo :|> t}

-- ** Find Keywords

scanKeywords :: T.Text -> State BotState ([T.Text], [Keyword])
scanKeywords input = case parse phrasesParser "" input of
    Left _  -> pure ([], [])
    Right slices -> foldrM analyzeKeywords ([], []) slices
 where
  analyzeKeywords phrase remainder =
    scanKwChunk phrase >>= pure . \case
      []  -> remainder
      kws -> (phrase, kws)

scanKwChunk :: [T.Text] -> State BotState [Keyword]
scanKwChunk wrds = do
  script <- gets botScript
  let loop [] stack _ = stack
      loop (w:ws) stack p = case findKeyword w script of
        Nothing -> loop ws stack p
        Just kw -> if kwPrecedence kw > p
                    then loop ws (kw S.:<| stack) (kwPrecedence kw)
                    else loop ws (stack S.:|> kw) p
  pure (toList $ loop wrds S.Empty (-1))

reflect :: [T.Text] -> State BotState [T.Text]
reflect ws = do
  script <- gets botScript
  let exchange w = maybe w id (findReflection w script)
  pure (fmap exchange ws)

disassemble :: [MatchingRule] -> T.Text -> MaybeT (State BotState) [T.Text]
disassemble rs input = do
  p <- lift (parserFromRule rs)
  liftMaybe (parseMaybe p input)

reassemble :: [ReassemblyRule] -> [T.Text] -> T.Text
reassemble rs ts = T.concat (fmap (assembler ts) rs)
 where
  assembler ws = \case
    ReturnText  t -> t
    ReturnIndex n -> ws !! (n-1)

keywordsMatcher :: Traversable t => t Keyword -> T.Text -> State BotState T.Text
keywordsMatcher kws input =
  let results = fmap (flip matchKeyword input) kws
      defaultResponse = pickDefaultSay
  in maybe defaultResponse pure =<< runMaybeT (asum results)

matchKeyword :: Keyword -> T.Text -> MaybeT (State BotState) T.Text
matchKeyword keyword = tryDecompRules (kwRules keyword)

tryDecompRules :: Traversable t => t Rule -> T.Text -> MaybeT (State BotState) T.Text
tryDecompRules rules input = asum (fmap (flip tryDecompRule input) rules)

tryDecompRule :: Rule -> T.Text -> MaybeT (State BotState) T.Text
tryDecompRule rule input =
  let mrules = getMatchingRules rule
  in do
    result <- disassemble mrules input
    recomp <- lift $ pickAny (getRecompRules rule)
    case recomp of
      RNewkey      -> mzero
      RKeyword t   -> tryOtherKeyword t
      RRule rrules -> pure $ reassemble rrules result
 where
  tryOtherKeyword t = do
     script <- gets botScript
     kw <- findKeyword t script
     matchKeyword kw input

-- * Generate decomposition parsers

parserFromRule :: [MatchingRule] -> State BotState (Parser [T.Text])
parserFromRule = fmap sequence . unfoldrM coalg
 where
  coalg [] = pure Nothing
  coalg (rule:rs) = fmap (\x -> Just (x,rs)) $ matchingRuleParser rule >>= \case
    Just p  -> pure p
    Nothing -> case rs of
      []     -> pure $ T.pack <$> manyTill anySingle eof
      (x:_)  -> matchingRuleParser x >>= pure . \case
        Just p  -> T.strip . T.pack <$> manyTill anySingle (lookAhead p)
        Nothing -> T.strip . T.pack <$> manyTill anySingle eof

matchingRuleParser :: MatchingRule -> State BotState (Maybe (Parser T.Text))
matchingRuleParser = runMaybeT . \case
  MatchWord   w  -> pure (exactWord w)
  MatchChoice ws -> pure $ choice (fmap exactWord ws)
  MatchGroup  g  -> do
    grps <-  gets (findGroup g . botScript)
    pure $ choice (fmap exactWord grps)
  MatchN      n  -> pure (T.unwords <$> count n word)
  MatchMany      -> mzero

-- | Chunk a text into phrases (break on delimiters)
--   and each phrase into words (break on spaces)
phrasesParser :: Parser [[T.Text]]
phrasesParser = sepEndBy phrase punctParser
 where phrase = space *> some (lexeme (T.pack <$> some validChar))

validChar :: Parser Char
validChar = satisfy (not . (\x -> isSeparator x || elem x puncts))
 where puncts = ".,!?;:" :: [Char]

punctParser :: Parser Char
punctParser = satisfy (flip elem (".,!?;:" :: [Char]))

-- * Random Picking

liftRandom :: (Random a) => (StdGen -> (a, StdGen)) -> State BotState a
liftRandom f = do
  (x, nseed) <- gets (f . botSeed)
  modify $ \bot -> bot{botSeed = nseed}
  pure x

pickRandomR :: (Random a) => (a, a) -> State BotState a
pickRandomR = liftRandom . randomR

pickRandom :: (Random a) =>  State BotState a
pickRandom = liftRandom random

pickAny :: V.Vector a -> State BotState a
pickAny v = pickRandomR (0, V.length v - 1) >>= V.indexM v

pickGreeting :: State BotState T.Text
pickGreeting = gets (greetings . botScript) >>= pickAny

pickGoodbye :: State BotState T.Text
pickGoodbye = gets (goodbyes . botScript) >>= pickAny

pickDefaultSay :: State BotState T.Text
pickDefaultSay = gets (defaultSays . botScript) >>= pickAny
