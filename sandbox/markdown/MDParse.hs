module MDParse where

import Control.Monad

import Parsers

-----------------------------------------------------------------
---------------------------Ограничения---------------------------
-----------------------------------------------------------------
--Блоки разделены одной пустой строкой
--Слова в строках -- одним пробелом

-------------------Data Types-------------------

type Document = [Block]

-- |Represents block entity
data Block = Blank
           | Header (Int,Inline)
           | Paragraph [Inline]
  deriving (Show,Eq)

-- |Represent inline entity, just a string for this moment  
data Inline = Plain String
            | Bold String
            | Italic String
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

-- TODO: Переименовать эту функцию
f :: Parser Inline
f = bold <|> italic <|> plain

-------------------Attemps to parse a line of text-------------------

-- |Парсит слова (plain, bold, italic), разделённые одним пробелом
-- Ломается, если, например, внутри plain есть * 
line :: Parser [Inline]
line = do
  --l <- sepby f (char ' ')
  l <- sepby f (many (char ' '))
  return l

-- works with:       (letter, many (char ' '))
--                   (word, char ' ')
-- don't work with : (word, many (char ' ')) 
line' = do
  l <- sepby word (many (char ' ')) 
  return l

line_test1 = (fst . head $ parse line "acb **abc**  _de_") == 
             [Plain "acb",Bold "abc",Plain "",Italic "de"]

-- Парсит несколько подряд идущих линий, не уверен насчёт типа этой функции
lines :: Parser [[Inline]]
lines = sepby line newline

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
  spaces
  hashes <- mmany (char '#') 
  spaces
  x <- plain
  spaces 
  --char '\n'
  return $ Header (length hashes,x)

-- |Parse paragraph

-----------------------------------------------------------------
-------------------Parsers for whole Document--------------------
-----------------------------------------------------------------

-- |Парсит документ и возвращает список блоков
doc :: Parser [Block]
doc = do
  h <- header
  ls <- MDParse.lines
  return $ h:[Paragraph . concat $ ls]