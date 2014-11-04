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

-- |Parse italic text (html <em>)
italic :: Parser Inline
italic = liftM Italic $  
  bracket (char '*') word (char '*') `mplus`
  bracket (char '_') word (char '_')  

-- |Parse bold text (html <strong>)  
bold :: Parser Inline
bold = do
  let asterisks = char '*' >> char '*'
  let underlines = char '_' >> char '_'
  x <- bracket asterisks word asterisks `mplus` 
       bracket underlines word underlines
  return . Bold $ x  

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