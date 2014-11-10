import System.Environment
import System.IO
import Control.Monad

import Parsers
import MDParse
--import HTMLGen

main = do
  [fname] <- getArgs
  raw <- readFile fname
  
  ------------markdown parsing experiments------------
  putStrLn $ "Raw contents of markdown file " ++ fname ++ ": "
  putStrLn raw
  --print $ parse (sepby line newline) . snd . head $ parse header raw
  let a = parse doc raw
  print a

  ------------html generation experiments------------
  --let b = fst . head $ parse bold "**bold**"
  --print $ genInline b
  --putStrLn "\nGenerated html: "
  --let html = serialize a
  --print $ html
  --writeFile "test.html" html