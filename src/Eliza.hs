{-# LANGUAGE OverloadedStrings #-}
module Eliza (
    module Eliza
  , module Script
  ) where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Data.Sequence   as S
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

import           Data.Foldable   (toList, asum)
import           Control.Arrow   ((&&&))
import           System.Random
import           Control.Monad.State

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import           Data.Char (isSeparator)

import Script

-- Auxiliar function
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f d = foldr (\x y -> f x =<< y) (pure d)

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
  phrase <- T.unwords <$> reflect ws
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
scanKwChunk ws = do
  script <- botScript <$> get
  let loop [] stack _ = stack
      loop (w:ws) stack p = case findKeyword w script of
        Nothing -> loop ws stack p
        Just kw -> if kwPrecedence kw > p
                    then loop ws (kw S.:<| stack) (kwPrecedence kw)
                    else loop ws (stack S.:|> kw) p
  pure (toList $ loop ws S.Empty (-1))

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
  -- TODO: State monad!!!
  pure $ do
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

{-
  Parser part
-}

-- Chunk a phrase into phrases made of words
phrasesParser :: Parser [[T.Text]]
phrasesParser = MP.sepEndBy phrase punctParser
 where phrase = MP.space *> MP.some (lexeme (T.pack <$> MP.some validChar))
       wordParser = fmap T.pack (MP.some validChar)

validChar :: Parser Char
validChar = MP.satisfy (not . (\x -> isSeparator x || elem x puncts))
 where puncts = ".,!?;:" :: [Char]

punctParser :: Parser Char
punctParser = MP.satisfy ((flip elem) (".,!?;:" :: [Char]))

-- Apply parser and report error in case of failure
parseReport p e t = maybe (error e) id (MP.parseMaybe p t)
