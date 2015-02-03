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

type Position = (Int, Int)

-- | Describes parsing state: inout string and current position
data TM.TextualMonoid t => 
  ParserState t = ParserState { position :: Position
                              , input  :: t
                              } deriving (Eq)

instance TM.TextualMonoid t => Show (ParserState t) where
  show st = "{pos = " ++ (show $ position st) ++ 
    ", input = \"" ++ (TM.foldr_ (:) (mempty) (input st)) ++ "\"}"

type ParseError = String 
  
type Parser t a = 
  ReaderT Position (StateT (ParserState t) (Either ParseError)) a

bad :: TM.TextualMonoid t =>
  ParserState t -> String -> Either ParseError (a,ParserState t) 
bad state msg = Left ((show $ position state) ++ " " ++ msg)

parse :: TM.TextualMonoid t => 
  Parser t a -> t -> Either ParseError (a,ParserState t)
parse p s = 
  runStateT (runReaderT p initPos) 
            (ParserState {input = s, position = initPos})
    where initPos = (1,1)
-- | Consumes one symbol of any kind
-- TODO: Продумать, как правильно отслеживать отступы и заложить это в парсере item
-- TODO: Попробовать упросить, длинновато получилось
item :: TM.TextualMonoid t => Parser t Char
item = do
  defpos <- ask -- asking for initial position of current definition 
  state  <- get -- geting current parser state
  -- trying to split input ((x:xs) analog)
  let s = TM.splitCharacterPrefix . input $ state
  guard $ onside (position state) defpos && isJust s
  let (c,rest) = fromJust s
  put (ParserState {position = updatePos (position state) c, input = rest})
  return c
    where
      onside :: Position -> Position -> Bool
      onside (l,c) (defl,defc) = (c >= defc) || (l == defl)

      updatePos :: Position -> Char -> Position
      updatePos (line, col) c =
        case c of 
          '\n' -> (line + 1,1)
          '\t' -> (line,((col `div` 8)+1)*8)
          _    -> (line,col + 1)  

-- Пропускатель пробелов
junk :: TM.TextualMonoid t => Parser t ()
junk = local (\_ -> (0,-1)) spaces


-- Попробуем написать комбинатор, который готовит 
-- парсер для разбора нового блока 
--off :: TM.TextualMonoid t => Parser t a -> Parser t a
--off p = do
--  (defl,defc) <- ask -- ask for current definition (kind of initial) position
--  ((l,c),_)   <- get -- get current position
--  guard $ c > defc
--  local (\_ -> (l,c)) p

--offside_test :: TM.TextualMonoid t => Parser t ()
--offside_test = do
--  digit
--  newline
--  many digit 
--  newline

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

newline :: TM.TextualMonoid t => Parser t ()
newline  = char '\n' >> return () 
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
spaces :: TM.TextualMonoid t => Parser t ()
spaces = many (sat isSpace) >> return ()

------------------Repetitions with seporators---------------- 
sepby :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

sepby1 :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
p `sepby1` sep = do 
  a <- p
  as <- many (sep >> p)
  return (a:as)
