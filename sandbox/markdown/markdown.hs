import Control.Monad

import Parsers

-------------------Data Types-------------------

-- |Represent inline entity, just a string for this moment  
data Inline = Plain String
            | Bold String
            | Italic String
  deriving (Show,Eq)

-- |Represents block entity
data Block = Blank
           | Header (String,String)
  deriving (Show,Eq) 

-----------------------------------------------------------------
-------------------Parsers for Inline elements-------------------
-----------------------------------------------------------------

-------------------Parsers for single word (word may be plain, bold or italic)-------------------

-- |Parse plain text
plain :: Parser Inline
plain = (liftM Plain) word

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

-- Вопрос о порядке применения парсеров 
f :: Parser Inline
f = bold `mplus` italic `mplus` plain

-------------------Attemps to parse a line of text-------------------

-- |Парсит слова (plain, bold, italic), разделённые пробелами
-- Ломается, если, например, внутри plain есть * 
line = sepby f (char ' ')

line_test1 = (fst . head $ parse line "acb **abc**  _de_") == 
             [Plain "acb",Bold "abc",Plain "",Italic "de"]

-----------------------------------------------------------------
-------------------Parsers for Block elements--------------------
-----------------------------------------------------------------

-- |Parse blank line
blank :: Parser Block
blank = do
  many (sat wspaceOrTab)
  char '\n'
  return Blank 
    where wspaceOrTab = (\x -> x == ' ' || x == '\t')

-- |Parse header
-- Пока в нашем заголовке только одно слово
header :: Parser Block 
header = do
  hashes <- many (char '#') 
  spaces
  x <- word
  spaces 
  return $ Header (hashes,x)

-- |Parse paragraph

-----------------------------------------------------------------
-------------------Parsers for whole Document--------------------
-----------------------------------------------------------------

-- |Парсит документ и возвращает список блоков
--doc :: Parser [Block]
--doc = do
--  x <- header `mplus` blank
--  return x
