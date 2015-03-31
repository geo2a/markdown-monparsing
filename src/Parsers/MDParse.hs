module MDParse where

import Control.Monad
import Control.Applicative
import qualified Data.Monoid.Textual as TM

import Parsers
import Helpers

-----------------------------------------------------------------
---------------------------Ограничения---------------------------
-----------------------------------------------------------------
-- Всех не перечесть, парсим пока только заголовки и параграфы простого текста, 
-- а также списки; вложенные конструкции не допускаются 

-------------------Data Types-------------------

type Document = [Block]

-- |Represents block entity
data Block = Blank
           | Header (Int,Line)
           | Paragraph [Line]
           | UnorderedList [Line]
           | BlockQuote [Line]
  deriving (Show,Eq)

-- |Represents line as list of inline elements (words)
data Line = Empty | NonEmpty [Inline]
  deriving (Show,Eq)

-- |Represent inline entity, just a string for this moment  
-- Что делать с пунктуацией и пробелами? 
data Inline = Plain String
            | Bold String
            | Italic String
            | Monospace String
  deriving (Show,Eq) 

-----------------------------------------------------------------
-------------------Parsers for Inline elements-------------------
-----------------------------------------------------------------

-------------------Helper Parsers-----------------
punctuation :: TM.TextualMonoid t => Parser t Char
punctuation = foldl1 (<|>) (map char marks) 
  where marks = ".,:;!?"

alphanumsWithPunctuation :: TM.TextualMonoid t => Parser t String
alphanumsWithPunctuation = some (alphanum <|> punctuation)

-- |Sequence of alphanums with punctuations and spaces
sentence :: TM.TextualMonoid t => Parser t String
sentence =
  some (do
    w <- alphanumsWithPunctuation 
    s <- (many (char ' ')) 
    return $ w ++ s
  ) >>= return . concat

-------------------Parsers for single word (word may be plain, bold or italic)-------------------

-- |Parse plain text
plain :: TM.TextualMonoid t => Parser t Inline
plain = do 
  txt <- sentence
  return . Plain $ txt
  
-- |Parse italic text (html <em>)
italic :: TM.TextualMonoid t => Parser t Inline
italic = do  
  txt <- bracket (char '*') sentence (char '*') <|>
         bracket (char '_') sentence (char '_')
  p   <- many punctuation
  return . Italic $ txt ++ p  

-- |Parse bold text (html <strong>)  
bold :: TM.TextualMonoid t => Parser t Inline
bold = do
  txt <- bracket asterisks sentence asterisks <|> 
       bracket underlines sentence underlines
  p   <- many punctuation
  return . Bold $ txt ++ p  
  where
    asterisks = char '*' >> char '*'
    underlines = char '_' >> char '_'  

monospace :: TM.TextualMonoid t => Parser t Inline
monospace = do
  txt <- bracket (char '`') sentence (char '`')
  p   <- many punctuation
  return . Monospace $ txt ++ p
-----------------------------------------------------------------
-------------------Parser for Lines-------------------
-----------------------------------------------------------------

-- |Парсит слова (plain, bold, italic), разделённые одним пробелом
-- Ломается, если, например, внутри plain есть * 
line :: TM.TextualMonoid t => Parser t Line
line = emptyLine `mplus` nonEmptyLine

emptyLine :: TM.TextualMonoid t => Parser t Line
emptyLine = many (sat wspaceOrTab) >> char '\n' >> return Empty

-- TODO: Получилось как-то сложно, подумать, как бы попроще
nonEmptyLine :: TM.TextualMonoid t => Parser t Line
nonEmptyLine = do
  many (sat wspaceOrTab)
  l <- sepby1 (bold <|> italic <|> plain <|> monospace) (many (char ' '))
  many (sat wspaceOrTab)
  char '\n'
  return . NonEmpty $ l

-----------------------------------------------------------------
-------------------Parsers for Block elements--------------------
-----------------------------------------------------------------

-- |Parse header
-- поддерживаются только заголовки в стиле #
header :: TM.TextualMonoid t => Parser t Block 
header = do
  many (sat wspaceOrTab) -- spaces, но без \n, TODO^: дать этой функции имя
  hashes <- token (some (char '#')) 
  text <- nonEmptyLine
  return $ Header (length hashes,text)

-- |Parse paragraph
paragraph :: TM.TextualMonoid t => Parser t Block
paragraph = do
  --l <- bracket emptyLine nonEmptyLine emptyLine
  l <- some nonEmptyLine
  return . Paragraph $ l

-- |Parse unordered list
unorderdList :: TM.TextualMonoid t => Parser t Block
unorderdList = do
  items <- some (token bullet >> line)
  return . UnorderedList $ items 
  where
    bullet :: TM.TextualMonoid t => Parser t Char
    bullet = char '*' <|> char '+' <|> char '-' >>= return

-- TODO: Эта функция делает почти тоже самое, что и emptyLine, 
-- TODO непонятно, как совместить их в одну, или, по крайней мере, 
-- TODO избежать дублирования кода
blank :: TM.TextualMonoid t => Parser t Block
blank = many (sat wspaceOrTab) >> char '\n' >> return Blank

-- |Parse blockquote
blockquote :: TM.TextualMonoid t => Parser t Block
blockquote = do
  lines <- some (token (char '>') >> line)
  return . BlockQuote $ lines  

-- |Черновик для латех-блоков
blockMath :: TM.TextualMonoid t => Parser t Block
blockMath = (bracket (string "$$") (some (sat (/= '$'))) (string "$$")) >>= 
  return . Paragraph . (: []) . NonEmpty . (: []) . Plain . 
    (\x -> "$$" ++ x ++ "$$") 

-----------------------------------------------------------------
-------------------Parsers for whole Document--------------------
-----------------------------------------------------------------

-- |Парсит документ и возвращает список блоков
doc :: TM.TextualMonoid t => Parser t Document
doc = do
  ls <- many block
  --a <- header
  --b <- blank
  --c <- paragraph
  return $ ls
  --return [a,b,c] 
  where 
    block = 
      blank <|> header <|> paragraph <|> 
      unorderdList <|> blockquote <|> blockMath