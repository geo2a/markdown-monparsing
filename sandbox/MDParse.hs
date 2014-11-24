module MDParse where

import Control.Monad

import Parsers
import Helpers

-----------------------------------------------------------------
---------------------------Ограничения---------------------------
-----------------------------------------------------------------
--Всех не перечесть, парсим пока только заголовки и параграфы простого текста

-------------------Data Types-------------------

type Document = [Block]

-- |Represents block entity
data Block = Blank
           | Header (Int,Line)
           | Paragraph [Line]
  deriving (Show,Eq)

-- |Represents line as list of inline elements (words)
data Line = Empty | NonEmpty [Inline]
  deriving (Show,Eq)

-- |Represent inline entity, just a string for this moment  
-- Что делать с пунктуацией и пробелами? 
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
line = emptyLine `mplus` nonEmptyLine

emptyLine :: Parser Line
emptyLine = many (sat wspaceOrTab) >> char '\n' >> return Empty

-- TODO: Получилось как-то сложно, подумать, как бы попроще
nonEmptyLine :: Parser Line
nonEmptyLine = do
  many (sat wspaceOrTab)
  l <- sepby1 (bold <|> italic <|> plain) (many (char ' '))
  many (sat wspaceOrTab)
  char '\n'
  return . NonEmpty $ l

line_test1 = 
  (fst . head $ parse nonEmptyLine "acb   **abc**  _de_\n") == 
    NonEmpty [Plain "acb",Bold "abc",Italic "de"]

-----------------------------------------------------------------
-------------------Parsers for Block elements--------------------
-----------------------------------------------------------------

-- |Parse header
-- Пока в нашем заголовке только одна строка, 
-- поддерживаются только заголовки в стиле #
header :: Parser Block 
header = do
  many (sat wspaceOrTab) -- spaces, но без \n, TODO^: дать этой функции имя
  hashes <- token (many1 (char '#')) 
  text <- nonEmptyLine
  return $ Header (length hashes,text)

-- |Parse paragraph
paragraph :: Parser Block
paragraph = do
  --l <- bracket emptyLine nonEmptyLine emptyLine
  l <- many1 nonEmptyLine
  return . Paragraph $ l

-- TODO: Эта функция делает почти тоже самое, что и emptyLine, 
-- TODO непонятно, как совместить их в одну, или, по крайней мере, 
-- TODO избежать дублирования кода
blank :: Parser Block
blank = many (sat wspaceOrTab) >> char '\n' >> return Blank

-----------------------------------------------------------------
-------------------Parsers for whole Document--------------------
-----------------------------------------------------------------

-- |Парсит документ и возвращает список блоков
doc :: Parser Document
doc = do
  --h <- header
  ls <- many1 (blank `mplus` header `mplus` paragraph)
  return $ ls