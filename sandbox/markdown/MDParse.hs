module MDParse where

import Control.Monad

import Parsers
import Helpers

-----------------------------------------------------------------
---------------------------Ограничения---------------------------
-----------------------------------------------------------------
--Блоки разделены одной пустой строкой
--Слова в строках -- одним пробелом

-------------------Data Types-------------------

type Document = [Block]

-- |Represents block entity
data Block = Blank
           | Header (Int,Line)
           | Paragraph [Line]
  deriving (Show,Eq)

type Line = [Inline]

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
-----------------------------------------------------------------
-------------------Parser for Lines-------------------
-----------------------------------------------------------------

-- |Парсит слова (plain, bold, italic), разделённые одним пробелом
-- Ломается, если, например, внутри plain есть * 
line :: Parser Line
line = do
  l <- sepby (bold <|> italic <|> plain) (many (char ' '))
  return l

line_test1 = (fst . head $ parse line "acb   **abc**  _de_") == 
             [Plain "acb",Bold "abc",Italic "de"]

-- Парсит несколько подряд идущих линий, не уверен насчёт типа этой функции
lines :: Parser Block
lines = do
  p <- sepby line newline
  return . Paragraph $ p

-----------------------------------------------------------------
-------------------Parsers for Block elements--------------------
-----------------------------------------------------------------

-- |Parse blank line
blank :: Parser Block
blank = do
  many (sat wspaceOrTab)
  char '\n'
  return Blank

-- |Parse header
-- Пока в нашем заголовке только одна строка, 
-- поддерживаются только заголовки в стиле #
header :: Parser Block 
header = do
  spaces
  hashes <- token (many (char '#')) 
  text <- line
  return $ Header (length hashes,text)

-- |Parse paragraph
--paragraph :: Parser Paragraph
--paragraph 
-----------------------------------------------------------------
-------------------Parsers for whole Document--------------------
-----------------------------------------------------------------

-- |Парсит документ и возвращает список блоков
doc :: Parser [Block]
doc = do
  h <- header
  ls <- MDParse.lines
  return $ h:[ls]