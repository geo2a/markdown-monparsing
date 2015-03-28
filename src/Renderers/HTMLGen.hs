module HTMLGen where

import Parsers
import MDParse

import System.Directory -- for demonstration 

documentHeader :: String -> String
documentHeader title = unlines [
  "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<title>" ++ title ++ "</title>",
    "<script type=\"text/x-mathjax-config\">",
      "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});",
    "</script>",
    "<script type=\"text/javascript\"",
      "src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">",
    "</script>",
    "</head>",
    "<body>"]

documentFooter :: String
documentFooter = unlines
  ["</body>","</head>"]

generateHTML :: String -> Document -> String
generateHTML title doc = 
  documentHeader title
  ++ serialize doc
  ++ documentFooter 

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

