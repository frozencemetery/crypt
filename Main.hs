module Main where

import Crypt.Parser (file)
import Text.Parsec (runParser)

main :: IO ()
main = do
    contents <- getContents
    print (runParser file () "<stdin>" contents)
