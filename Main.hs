module Main where

import Crypt.Parser     (file)
import Text.Parsec      (runParser)
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
    contents <- getContents
    putStrLn $ ppShow $ runParser file () "<stdin>" contents
