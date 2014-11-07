import System.Environment
import Control.Monad

import Parsers
import MDParse
 

main = do
  [fname] <- getArgs
  raw <- readFile fname
  putStrLn $ "Raw contents of markdown file " ++ fname ++ ": "
  --print $ parse (sepby line newline) . snd . head $ parse header raw
  print $ parse g raw

g :: Parser [Block]
g = do
  h <- header
  ls <- MDParse.lines
  return $ h:[Paragraph . concat $ ls]
