import System.Environment
import System.IO
import Control.Monad

import Parsers
import MDParse
import HTMLGen

data OutputFormat = HTML

outputName :: OutputFormat -> String
outputName HTML = "output.html"

errorLogFile :: String
errorLogFile = "error.log"

main = do 
  [fname] <- getArgs
  raw <- readFile fname
  putStrLn $ "\nGenerated html, will be written to " ++ outputName HTML ++ ": " 
  case (parse doc raw) of 
    Right tree -> do
      --putStrLn $ "Parsed markdown: " ++ show(tree)
      let html = generateHTML (outputName HTML) . fst $ tree
      --print $ html
      writeFile (outputName HTML) html 
    Left err -> 
      writeFile (show err) errorLogFile