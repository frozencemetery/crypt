module Main where

import qualified Tests.Parser as Parser

import Test.Framework (defaultMain)

main = defaultMain [ Parser.tests ]
