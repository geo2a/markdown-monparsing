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

main = do 
  --[fname] <- getArgs
  raw <- readFile inputFName
  
  ------------markdown parsing experiments------------
  putStrLn $ "Raw contents of markdown file " ++ inputFName ++ ": "
  putStrLn raw
  let a = parse doc raw
  print a

  ------------html generation experiments------------
  putStrLn $ "\nGenerated html, will be written to " ++ outputFName ++ ": " 
  case a of 
    Right doc -> do
      let html = generateHTML "test" . fst $ doc
      print $ html
      writeFile outputFName html 
    Left err -> 
      print err 