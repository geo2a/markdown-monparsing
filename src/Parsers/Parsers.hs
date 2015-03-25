
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parsers where

import Prelude hiding (splitAt)
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
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

data ParseError = EmptyInput | UnsatisfiedPredicate String
  deriving (Show, Eq)
  
type ErrorReport t = (ParseError, (ParserState t))

newtype Parser t a = Parser (  
    ReaderT Position (StateT (ParserState t) (Either (ErrorReport t))) a
  ) deriving (Functor, Applicative, Monad,
              MonadReader Position,
              MonadState (ParserState t)
              , MonadError (ErrorReport t)
              )

parse :: TM.TextualMonoid t => 
  Parser t a -> t -> Either (ErrorReport t) (a,ParserState t)
parse (Parser p) s = 
  runStateT (runReaderT p initPos) 
            (ParserState {input = s, position = initPos})
    where initPos = (1,1)
-- | Consumes one symbol of any kind
-- TODO: Продумать, как правильно отслеживать отступы и заложить это в парсере item
-- TODO: Попробовать упросить, длинновато получилось
item :: TM.TextualMonoid t => Parser t Char
item = do
  defpos <- ask
  state  <- get
  -- trying to split input ((x:xs) analog)
  let s = TM.splitCharacterPrefix . input $ state
  case s of 
    Nothing -> throwError (EmptyInput,state)
    Just (c,rest) -> do  
      let (c,rest) = fromJust s
      put (ParserState {position = updatePos (position state) c, input = rest})
      return c
    where
      updatePos :: Position -> Char -> Position
      updatePos (line, col) c =
        case c of 
          '\n' -> (line + 1,1)
          '\t' -> (line,((col `div` 8)+1)*8)
          _    -> (line,col + 1)  

-- |Consumes item only if it satisfies predicate
sat :: TM.TextualMonoid t => (Char -> Bool) -> Parser t Char
sat p = do
  x <- item 
  if p x then return x else get >>= \state -> 
    throwError (UnsatisfiedPredicate "general",state)

--------------------Парсеры для одиночных символов----------------

-- |Consumes item only if it is equal to specified char
char :: TM.TextualMonoid t => Char -> Parser t Char
char x = do
  state <- get
  sat (\y -> x == y) `catchError` \(UnsatisfiedPredicate _,_) -> 
    throwError (UnsatisfiedPredicate "char",state)

-- |Decimal digit
digit :: TM.TextualMonoid t => Parser t Char
digit = do
  state <- get
  sat isDigit `catchError` \(UnsatisfiedPredicate _,_) -> 
    throwError (UnsatisfiedPredicate "digit",state)

-- |Lowercase letter
lower :: TM.TextualMonoid t => Parser t Char
lower = do
  state <- get
  sat isLower `catchError` \(UnsatisfiedPredicate _,state) -> 
    throwError (UnsatisfiedPredicate "lower",state)

-- |Uppercase letter
upper :: TM.TextualMonoid t => Parser t Char
upper = do
  state <- get
  sat isUpper `catchError` \(UnsatisfiedPredicate _,state) ->
    throwError (UnsatisfiedPredicate "upper",state)

-- |Anycase letter
-- TODO: обобщить отлов ошибок
letter :: TM.TextualMonoid t => Parser t Char
letter = do
  state <- get
  lower `catchError` \(UnsatisfiedPredicate _,_) -> 
    upper `catchError` \(UnsatisfiedPredicate _,_) ->
      throwError (UnsatisfiedPredicate "letter",state)

---- |Anycase letter or decimal digit
alphanum :: TM.TextualMonoid t => Parser t Char
alphanum = do
  state <- get
  letter `catchError` \(UnsatisfiedPredicate _,_) -> 
    digit `catchError` \(UnsatisfiedPredicate _,_) -> 
      throwError (UnsatisfiedPredicate "alphanum",state)

newline :: TM.TextualMonoid t => Parser t ()
newline  = char '\n' `catchError` handler >> return ()
  where handler (UnsatisfiedPredicate _,state) = 
          throwError (UnsatisfiedPredicate "newline",state) 
----------------------Парсеры для групп символов----------------

-- |Parse a specified string
string :: TM.TextualMonoid t => String -> Parser t String
string s = do
  state <- get
  (mapM char s) `catchError` \(UnsatisfiedPredicate _,_) ->
    throwError (UnsatisfiedPredicate ("string " ++ s),state) 

------ |Word (non-empty string of letters)
----word :: TM.TextualMonoid t => Parser t String 
----word = some letter 

------ |Like word, but may contain digits
----alphanums :: TM.TextualMonoid t => Parser t String 
----alphanums = some alphanum

------ |Parse a token with specific parser, throw away any trailing spaces
----token :: TM.TextualMonoid t => Parser t a -> Parser t a
----token p = spaces >> p

------ |Parse a symbolic token, just a specification of token parser
----symbol :: TM.TextualMonoid t => String -> Parser t String
----symbol cs = token (string cs)

------ |Parse a thing enclosed by brackets
----bracket :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t c -> Parser t b
----bracket open p close = do 
----  open
----  x <- p
----  close
----  return x

----------------------"Lexical issues"----------------
----spaces :: TM.TextualMonoid t => Parser t ()
----spaces = many (sat isSpace) >> return ()

----------------------Repetitions with seporators---------------- 
----sepby :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
----p `sepby` sep = (p `sepby1` sep) <|> return []

----sepby1 :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
----p `sepby1` sep = do 
----  a <- p
----  as <- many (sep >> p)
----  return (a:as)
