import Control.Monad

import Parsers

-------------------Data Types-------------------

-- |Represent inline entity, just a string for this moment  
data Inline = String
  deriving (Show)

-- |Represents block entity
data Block = Header (String,String)
           | Plain String
  deriving (Show) 

-- |Get header text
header :: Parser Block 
header = do
  sharps <- many (char '#') 
  spaces
  x <- many alphanum
  return $ Header (sharps,x)