module Parsers where

import Prelude hiding (splitAt)
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust, isJust)
import Data.Monoid (mempty, mappend, mconcat)
import qualified Data.Monoid.Textual as TM
import Data.Char

type Pos = (Int, Int)

-- | Describes parsing state: inout string and current position
type PState t = (Pos, t)

type Parser t a = ReaderT Pos (StateT (PState t) []) a

-- | TODO: Проблема с семантикой MonadPlus/Alternative
--   По умолчанию, mplus для списка реализуется как ++, видимо
--   Нужно перегружить инстанс для MonadPlus. Использовать newtype?

parse :: TM.TextualMonoid t => Pos -> Parser t p -> t -> [(p,(PState t))]
parse defpos p s = runStateT (runReaderT p defpos) (defpos,s)
-- | Consumes one symbol of any kind 
--   item parser now fails if the position of the character to be consumed
--   is not onside with respect to current definition position
--   A position is onside if its column number is strictly greater than the current defi-
--   nition column. However, the first character of a new definition begins in the same
--   column as the definition column, so this is handled as a special case
-- TODO: Продумать, как правильно отслеживать отступы и заложить это в парсере item
-- TODO: Попробовать упросить, длинновато получилось
item :: TM.TextualMonoid t => Parser t Char
item = do
  defpos <- ask -- asking for initial position of current definition 
  (pos, input) <- get -- geting current input stream state
  let s = TM.splitCharacterPrefix input -- trying to split input ((x:xs) analog)  
  guard $ onside pos defpos && isJust s
  let (c,rest) = fromJust s
  put (updatePos pos c,rest) 
  return c
    where
      onside :: Pos -> Pos -> Bool
      onside (l,c) (dl,dc) = (c >= dc) || (l == dl)

      updatePos :: Pos -> Char -> Pos
      updatePos (line, col) c =
        case c of 
          '\n' -> (line + 1,0)
          '\t' -> (line,((col `div` 8)+1)*8)
          _    -> (line,col + 1)  

-- | Old version -- without offside rule handling
--item :: TM.TextualMonoid t => Parser t Char
--item = do
--  s <- TM.splitCharacterPrefix `fmap` get
--  guard $ isJust s 
--  let (c,rest) = fromJust s
--  put rest
--  return c

-- |Consumes item only if it satisfies predicate
sat :: TM.TextualMonoid t => (Char -> Bool) -> Parser t Char
sat p = do
  x <- item 
  guard $ p x
  return x

------------------Парсеры для одиночных символов----------------

-- |Consumes item only if it is equal to specified char
char :: TM.TextualMonoid t => Char -> Parser t Char
char x = sat (\y -> x == y)

---- |Decimal digit
digit :: TM.TextualMonoid t => Parser t Char
digit = sat isDigit

-- |Lowercase letter
lower :: TM.TextualMonoid t => Parser t Char
lower = sat isLower

-- |Uppercase letter
upper :: TM.TextualMonoid t => Parser t Char
upper = sat isUpper

-- |Anycase letter
letter :: TM.TextualMonoid t => Parser t Char
letter = lower <|> upper

-- |Anycase letter or decimal digit
alphanum :: TM.TextualMonoid t => Parser t Char
alphanum = letter <|> digit

newline :: TM.TextualMonoid t => Parser t Char
newline  = char '\n'  
------------------Парсеры для групп символов----------------

-- |Parse a specified string
string :: TM.TextualMonoid t => String -> Parser t String
string = mapM char

-- |Word (non-empty string of letters)
word :: TM.TextualMonoid t => Parser t String 
word = some letter 

-- |Like word, but may contain digits
alphanums :: TM.TextualMonoid t => Parser t String 
alphanums = some alphanum

-- |Parse a token with specific parser, throw away any trailing spaces
token :: TM.TextualMonoid t => Parser t a -> Parser t a
token p = spaces >> p

-- |Parse a symbolic token, just a specification of token parser
symbol :: TM.TextualMonoid t => String -> Parser t String
symbol cs = token (string cs)

-- |Parse a thing enclosed by brackets
bracket :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t c -> Parser t b
bracket open p close = do 
  open
  x <- p
  close
  return x

------------------"Lexical issues"----------------
spaces :: TM.TextualMonoid t => Parser t String
spaces = many (sat isSpace)

------------------Repetitions with seporators---------------- 
sepby :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

sepby1 :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
p `sepby1` sep = do 
  a <- p
  as <- many (sep >> p)
  return (a:as)
