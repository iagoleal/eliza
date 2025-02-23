module Utils where

import Control.Applicative hiding (some, many)
import Control.Monad.State

import qualified Data.Text as T
import Data.Void (Void)

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map as M

-- * Monadic recursion schemes

-- | Monadic version of 'foldr', the list catamorphism.
-- Folds from the right.
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f d = foldr (\x y -> f x =<< y) (pure d)

-- | Monadic version of 'unfoldr', the list anamorphism.
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f seed =
  f seed >>= maybe (pure []) (\(a,b) -> (a:) <$> unfoldrM f b)

-- * Lift Monads

-- | Generalize a 'Maybe' value into any "Alternative'.
liftMaybe :: (Alternative m) => Maybe a -> m a
liftMaybe = maybe empty pure

-- | Generalize a 'Control.Monad.State.State' into any 'Control.Monad.State.MonadState'.
hoistState :: MonadState s m => State s a -> m a
hoistState = state . runState

-- | Like "Data.Map.lookup' but for any 'Control.Monad.MonadPlus'
genericLookup :: (Alternative m, Ord k) => k -> M.Map k v -> m v
genericLookup k m = liftMaybe $ M.lookup k m

-- * Safe versions of functions

-- | Safe version of 'Data.Text.head'
safeTextHead :: T.Text -> Maybe Char
safeTextHead = fmap fst . T.uncons

-- * General Parsers

-- | Megaparsec Parser used throught this program
type Parser = Parsec Void T.Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol' spaceConsumer

-- | Parser to match a word exactly,
-- ensuring that it is the complete word.
exactWord :: T.Text -> Parser T.Text
exactWord w = lexeme (string' w <* notFollowedBy alphaNumChar)

-- | Parser that matches any word.
word :: Parser T.Text
word = lexeme $ T.pack <$> some (alphaNumChar <|> char '\'' <|> char '-')

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Porser for positive (strictly greater than zero) integers
positiveInteger :: Integral a => Parser a
positiveInteger = do
  d <- lexeme L.decimal
  if d == 0
   then fail "Zero index not allowed"
   else pure d
