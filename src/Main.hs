{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import qualified Data.Sequence   as S
import           Data.List       (find)
import           Data.Foldable   (toList)
import           Data.Bifunctor

type Token = T.Text

type DecompRule = [MatchingRule]
type RecompRule = [MatchingRule]
type Rule = (DecompRule, [RecompRule])
data MatchingRule = MatchAll
                  | MatchWord Token
                  | MatchN Int
  deriving Show

data Keyword = Keyword { kwToken :: Token
                       , kwPrecedence :: Int
                       , kwRules :: [Rule]
                       }
  deriving Show


reflections :: M.Map T.Text T.Text
reflections = M.fromList
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
   ("me", "you")]

keywords :: [Keyword]
keywords = [ Keyword "happy" 5 []
           , Keyword "me"    4 []
           , Keyword "sad"  10 []
           ]

tokenize :: T.Text -> [Token]
tokenize = T.words . T.toLower

reflect :: [Token] -> [Token]
reflect = fmap reflectToken
  where
   reflectToken word = case M.lookup word reflections of
     Nothing -> word
     Just x  -> x

scanKeywords :: [Token] -> [Keyword]
scanKeywords l = toList $ loop l S.Empty (-1)
 where
  loop []     stack _ = stack
  loop (w:ws) stack p =
    if w == "." || w == ","
     then if null stack
           then loop ws stack p
           else stack
     else case find ((==w) . kwToken) keywords of
       Nothing -> loop ws stack p
       Just kw -> if (kwPrecedence kw) > p
                   then loop ws (kw S.:<| stack) (kwPrecedence kw)
                   -- Should use a Seq here
                   else loop ws (stack S.:|> kw) p


match :: [Keyword] -> [Token] -> T.Text
match [] s = defaultScript
match (k:ks) s = case matchRules (kwRules k) s of
  Nothing -> match ks s
  Just response  -> response

matchRules :: [Rule] -> [Token] -> Maybe T.Text
matchRules [] _ = Nothing
matchRules (r:rs) s =  case decompose dRule s of
  Nothing -> matchRules rs s
  Just x  -> Just (recompose rRule s)
 where (dRule, rRule) = second head r


decompose :: DecompRule -> [Token] -> Maybe [Token]
decompose d ts = decompose' d ts []
  where
   decompose' [] [] res = Just res
   decompose' [] (x:xs) _ = Nothing
   decompose' (r:rs) [] res = Just res
   decompose' (r:rs) t@(x:xs) res = case r of
     MatchWord w -> if w == x
                     then decompose' rs xs (res ++ [x])
                     else Nothing
     MatchN n    -> if length t >= n
                     then let (a, b) = splitAt n t
                          in decompose' rs b (res ++ a)
                     else Nothing
     MatchAll    -> if null rs
                     then Just (res ++ t)
                     else undefined

recompose :: RecompRule -> [Token] -> T.Text
recompose = undefined

defaultScript :: T.Text
defaultScript = "How do you feel about that?"

eliza :: T.Text -> T.Text
eliza input =
  let tokens   = reflect (tokenize input)
      keywords = scanKeywords tokens
  in match keywords tokens


main :: IO ()
main = putStrLn "Hello, I'm Eliza, your new therapist. How are you feeling today?"
