import System.Environment
import System.IO
import Control.Monad

import Parsers
import MDParse
import HTMLGen

inputFName :: FilePath
inputFName = "sandbox/md_to_html_usage/test.md"

outputFName :: FilePath
outputFName = "sandbox/md_to_html_usage/test.html"

inp = unlines [
    "# An h1 header",
    "",
    "Paragraphs are separated by a blank line"
  ]

main = do 
  --[fname] <- getArgs
  raw <- readFile inputFName
  
  ------------markdown parsing experiments------------
  putStrLn $ "Raw contents of markdown file " ++ inputFName ++ ": "
  putStrLn raw
  let a = parse doc raw

  ------------html generation experiments------------
  putStrLn $ "\nGenerated html, will be written to " ++ outputFName ++ ": " 
  case a of 
    Right tree -> do
      putStrLn $ "Parsed markdown: " ++ show(tree)
      let html = generateHTML "test" . fst $ tree
      print $ html
      writeFile outputFName html 
    Left err -> 
      print err 