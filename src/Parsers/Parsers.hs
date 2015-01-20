module Parsers where

import Prelude hiding (splitAt)
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust, isJust)
import Data.Monoid (mempty, mappend, mconcat)
import qualified Data.Monoid.Textual as TM
import Data.Char

type Parser t a = StateT t [] a

-- | TODO: Проблема с семантикой MonadPlus/Alternative
--   По умолчанию, mplus для списка реализуется как ++, видимо
--   Нужно перегружить инстанс для MonadPlus. Использовать newtype?

parse :: TM.TextualMonoid t => Parser t p -> t -> [(p,t)]
parse = runStateT 

-- |Consumes one symbol of any kind 
-- TODO: Попробовать упросить, длинновато получилось
item :: TM.TextualMonoid t => Parser t Char
item = do
  s <- TM.splitCharacterPrefix `fmap` get
  guard $ isJust s 
  let (c,rest) = fromJust s
  put rest
  return c

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
