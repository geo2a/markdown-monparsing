module HTMLGen where

import Parsers
import MDParse

serialize :: Document -> String
serialize = concatMap genBlock  

-- Не очень понятно, в какой момент добавлять \n

genBlock :: Block -> String
genBlock Blank = "\n"
genBlock (Header h) = 
  "<h" ++ s ++ ">" ++ genInline (snd h) ++ "</h" ++ s ++ ">" ++ "\n"
  where s = show (fst h)
genBlock (Paragraph p) = 
  "<p>" ++ concatMap genInline p ++ "</p>" ++ "\n" 

genInline :: Inline -> String
genInline (Plain s) = s
genInline (Bold s) = "<strong>" ++ s ++ "</strong>"
genInline (Italic s) = "<em>" ++ s ++ "</em>"
