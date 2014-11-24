module HTMLGen where

import Parsers
import MDParse

serialize :: Document -> String
serialize = concatMap genBlock

-- Не очень понятно, в какой момент добавлять \n

genBlock :: Block -> String
genBlock Blank = "\n"
genBlock (Header h) = 
  "<h" ++ s ++ ">" ++ genLine (snd h) ++ "</h" ++ s ++ ">" ++ "\n"
  where s = show (fst h)
genBlock (Paragraph p) = 
  "<p>" ++ concatMap genLine p ++ "</p>" ++ "\n" 

genLine :: Line -> String
genLine Empty        = ""
genLine (NonEmpty []) = genLine Empty
genLine (NonEmpty (l:ls)) = genInline l ++ " " ++  genLine (NonEmpty ls)   

genInline :: Inline -> String
genInline (Plain s) = s
genInline (Bold s) = "<strong>" ++ s ++ "</strong>"
genInline (Italic s) = "<em>" ++ s ++ "</em>"
