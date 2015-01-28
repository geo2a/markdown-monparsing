module HTMLGen where

import Parsers
import MDParse

import System.Directory -- for demonstration 

serialize :: Document -> String
serialize = concatMap genBlock

genBlock :: Block -> String
genBlock Blank = "\n"
genBlock (Header h) = 
  "<h" ++ s ++ ">" ++ genLine (snd h) ++ "</h" ++ s ++ ">" ++ "\n"
    where s = show (fst h)
genBlock (Paragraph p) = 
  "<p>" ++ concatMap genLine p ++ "</p>" ++ "\n"
genBlock (UnorderedList l) = 
  "<ul>" ++ concatMap ((++ "\n") . genOrderedListItem) l ++ "</ul>" ++ "\n"

genLine :: Line -> String
genLine Empty        = ""
genLine (NonEmpty []) = genLine Empty ++ "\n"
genLine (NonEmpty l) = concatMap ((++ " ") . genInline) l    

genOrderedListItem :: ListItem -> String
genOrderedListItem l = "<li>" ++ genLine l ++ "</li>" 

genInline :: Inline -> String
genInline (Plain s) = s
genInline (Bold s) = "<strong>" ++ s ++ "</strong>"
genInline (Italic s) = "<em>" ++ s ++ "</em>"

testMDtoHTML :: IO ()
testMDtoHTML = do 
  path <- getCurrentDirectory 
  html <- (serialize . fst . head . parse doc) `fmap` 
    readFile (path ++ "/sandbox/md_to_html_usage/test.md")
  writeFile (path ++ "/sandbox/md_to_html_usage/test.html" ) html

