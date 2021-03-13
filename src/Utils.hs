module Utils where

import Control.Applicative hiding (some, many)
import Control.Monad.State

import qualified Data.Text as T
import Data.Void (Void)

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M

-- Monadic recursion schemes
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f d = foldr (\x y -> f x =<< y) (pure d)

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f seed =
  f seed >>= maybe (pure []) (\(a,b) -> pure . (a:) =<< unfoldrM f b)

-- Lift Monads

liftMaybe :: (Alternative m) => Maybe a -> m a
liftMaybe = maybe empty pure

hoistState :: State s a -> StateT s IO a
hoistState = state . runState

genericLookup :: (Alternative m, Ord k) => k -> M.Map k v -> m v
genericLookup k m = liftMaybe $ M.lookup k m

-- *Safe functions

safeTextHead :: T.Text -> Maybe Char
safeTextHead = fmap fst . T.uncons


-- General Parsers

type Parser = Parsec Void T.Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol' spaceConsumer

exactWord :: T.Text -> Parser T.Text
exactWord w = lexeme (string' w <* notFollowedBy alphaNumChar)

word :: Parser T.Text
word = lexeme $ T.pack <$> some (alphaNumChar <|> char '\'' <|> char '-')

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

positiveInteger :: Parser Int
positiveInteger = do
  d <- lexeme L.decimal
  if d == 0
   then fail "Zero index not allowed"
   else pure d
