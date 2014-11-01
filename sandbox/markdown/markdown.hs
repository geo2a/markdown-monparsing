import Control.Monad

import Parsers

-------------------Data Types-------------------

-- |Represent inline entity, just a string for this moment  
data Inline = Plain String
            | Bold String
            | Italic String
  deriving (Show)

-- |Represents block entity
data Block = Header (String,String)
  deriving (Show) 

-------------------Parsers for emphasis words (like bold)-------------------

-- Неготово
emphasised :: [Char] -> Parser Inline
emphasised c =
  twoChars '*' `mplus` twoChars '_'
  x <- word
  twoChars '*' `mplus` twoChars '_'
  return . Bold $ x
    where
      twoChars c = do
        char c

--bold :: Parser Inline
--bold = do
--  twoChars '*' `mplus` twoChars '_'
--  x <- word
--  twoChars '*' `mplus` twoChars '_'
--  return . Bold $ x
--    where
--      twoChars c = do
--        char c
        --char c

f :: Parser Inline
f = do
  words <- sepby1 word spaces 
  --words <- many (spaces `mplus` word `mplus` spaces)
  return . Plain . concat $ words 


-- |Get header text
header :: Parser Block 
header = do
  sharps <- many (char '#') 
  spaces
  x <- many alphanum
  return $ Header (sharps,x)