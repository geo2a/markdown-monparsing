
{-# LANGUAGE GeneralizedNewtypeDeriving
             , FlexibleInstances #-}

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
                              , remainder  :: t
                              } deriving (Eq)

brokenState :: TM.TextualMonoid t => ParserState t
brokenState = ParserState (-1,-1) mempty

instance TM.TextualMonoid t => Show (ParserState t) where
  show st = "{pos = " ++ (show $ position st) ++ 
    ", remainder = \"" ++ (TM.foldr_ (:) (mempty) (remainder st)) ++ "\"}"

data ParseError = Undefined String 
                | EmptyRemainder String 
                | UnsatisfiedPredicate String
                | BracketError String
  deriving (Show, Eq)
  
type ErrorReport t = (ParseError, (ParserState t))

newtype Parser t a = Parser (  
    StateT (ParserState t) (Either (ErrorReport t)) a
  ) deriving (Functor, Applicative, Monad,
              MonadState (ParserState t)
              , MonadError (ErrorReport t)
              )

--Потенциально проблемное место
instance TM.TextualMonoid t => MonadPlus (Parser t) where
  mzero = throwError (Undefined "", brokenState)
  ma `mplus` mb = 
    catchError ma $ \ea ->
      catchError mb $ \eb -> 
        throwError eb

instance TM.TextualMonoid t => Alternative (Parser t) where
  empty = mzero
  (<|>) = mplus

parse :: TM.TextualMonoid t => 
  Parser t a -> t -> Either (ErrorReport t) (a,ParserState t)
parse (Parser p) s = 
  runStateT p (ParserState {remainder = s, position = initPos})
    where initPos = (1,1)

reportError :: TM.TextualMonoid t => 
  Either (ErrorReport t) (a,ParserState t) -> String
reportError (Right _) = "Parsed successfully"
reportError (Left (err, ParserState pos inp)) = 
  "Parse error occured at " ++ show pos ++ ": "++ show err

-- | Consumes one symbol of any kind
-- TODO: Продумать, как правильно отслеживать отступы и заложить это в парсере item
-- TODO: Попробовать упросить, длинновато получилось
item :: TM.TextualMonoid t => Parser t Char
item = do
  state  <- get
  -- trying to split remainder ((x:xs) analog)
  let s = TM.splitCharacterPrefix . remainder $ state
  case s of 
    Nothing -> throwError (EmptyRemainder "item",state)
    Just (c,rest) -> do  
      let (c,rest) = fromJust s
      put (ParserState {position = updatePos (position state) c, remainder = rest})
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
  state <- get
  x <- item `overrideError` (EmptyRemainder "sat")
  if p x then return x else 
    throwError (UnsatisfiedPredicate "general",state)

--------------------Парсеры для одиночных символов----------------

-- |Helper function, overrides error that may occur in parser p 
-- |with custom message 
overrideError :: Parser t a -> ParseError -> Parser t a
overrideError p err = do
  state <- get
  p `catchError` \(oldErr, oldState) ->
    case oldErr of
      EmptyRemainder s -> throwError (EmptyRemainder s, state)
      _ -> throwError (err, state)
  where
    getMsg (Undefined s) = s
    getMsg (EmptyRemainder s) = s
    getMsg (UnsatisfiedPredicate s) = s
    getMsg (BracketError s) = s

---- |Consumes item only if it is equal to specified char
char :: TM.TextualMonoid t => Char -> Parser t Char
char x = (sat (== x)) `overrideError` 
  (UnsatisfiedPredicate ("char " ++ [x]))

-- |Decimal digit
digit :: TM.TextualMonoid t => Parser t Char
digit = sat isDigit `overrideError`  
  (UnsatisfiedPredicate "digit")

-- |Lowercase letter
lower :: TM.TextualMonoid t => Parser t Char
lower = sat isLower `overrideError` 
  (UnsatisfiedPredicate "lower")

-- |Uppercase letter
upper :: TM.TextualMonoid t => Parser t Char
upper = sat isUpper `overrideError`
    (UnsatisfiedPredicate "upper")

-- |Anycase letter
-- TODO: обобщить отлов ошибок
letter :: TM.TextualMonoid t => Parser t Char
letter = do
  state <- get
  lower <|> upper
  --lower `catchError` \(UnsatisfiedPredicate _,_) -> 
  --  upper `catchError` \(UnsatisfiedPredicate _,_) ->
  --    throwError (UnsatisfiedPredicate "letter",state)

---- |Anycase letter or decimal digit
alphanum :: TM.TextualMonoid t => Parser t Char
alphanum = do
  state <- get
  letter <|> digit
  --letter `catchError` \(UnsatisfiedPredicate _,_) -> 
  --  digit `catchError` \(UnsatisfiedPredicate _,_) -> 
  --    throwError (UnsatisfiedPredicate "alphanum",state)

newline :: TM.TextualMonoid t => Parser t ()
newline  = char '\n' `overrideError` 
  (UnsatisfiedPredicate "newline") >> return ()

------------------------Парсеры для групп символов----------------

-- |Parse a specified string
string :: TM.TextualMonoid t => String -> Parser t String
string s = do
  state <- get
  (mapM char s) `overrideError` 
    (UnsatisfiedPredicate ("string " ++ s))

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
  open `overrideError` (BracketError "invalid opening bracket") 
  x <- p
  close `overrideError` (BracketError "invalid closing bracket")
  return x

--------------------"Lexical issues"----------------
spaces :: TM.TextualMonoid t => Parser t ()
spaces = many (sat isSpace) >> return ()

--------------------Repetitions with seporators---------------- 
sepby :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

sepby1 :: TM.TextualMonoid t => Parser t a -> Parser t b -> Parser t [a]
p `sepby1` sep = do 
  a <- p
  as <- many (sep >> p)
  return (a:as)
