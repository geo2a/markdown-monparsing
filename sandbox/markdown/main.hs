import System.Environment
import System.IO
import Control.Monad

import Parsers
import MDParse
import HTMLGen

main = do
  [fname] <- getArgs
  raw <- readFile fname
  
  ------------markdown parsing experiments------------
  putStrLn $ "Raw contents of markdown file " ++ fname ++ ": "
  print $ parse (sepby line newline) . snd . head $ parse header raw
  let a = fst . head $ parse g raw

  ------------html generation experiments------------
  --let b = fst . head $ parse bold "**bold**"
  --print $ genInline b
  print $ serialize a
  writeFile "test.html" (serialize a)

g :: Parser [Block]
g = do
  h <- header
  ls <- MDParse.lines
  return $ h:[Paragraph . concat $ ls]
