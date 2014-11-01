import Control.Monad

import Parsers

----------------Parsers for lambdas----------------
data Expr = App Expr Expr         -- application
          | Lam String Expr       -- lambda abstraction
          | Let String Expr Expr  -- local defenition
          | Var String            -- variable
          deriving(Show)

expr :: Parser Expr
expr = atom `chainl1` (return App)

atom :: Parser Expr
atom = lam `mplus` local `mplus` var `mplus` paren

lam :: Parser Expr
lam = do 
  symbol "\\"
  x <- variable
  symbol "->"
  e <- expr
  return $ Lam x e  

local :: Parser Expr
local = do
  symbol "let"
  x <- variable
  symbol "="
  e <- expr
  symbol "in"
  e' <- expr
  return $ Let x e e'

var :: Parser Expr
var = do
  x <- variable
  return $ Var x

paren :: Parser Expr
paren = bracket (symbol "(") expr (symbol ")")

variable :: Parser String
variable = identifier ["let","in"]
