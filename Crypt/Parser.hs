module Crypt.Parser where

import Control.Applicative ((<*), (<|>))
import Control.Monad (void)
import qualified Text.Parsec.Expr as P
import qualified Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec (Parser)

import Crypt.Ast

-- lexer

langDef = P.LanguageDef
    { P.commentStart = "/*"
    , P.commentEnd = "*/"
    , P.commentLine = "//"
    , P.nestedComments = False
    , P.identStart = P.letter <|> P.char '_'
    , P.identLetter = P.letter <|> P.char '_' <|> P.digit
    , P.opStart = opChar
    , P.opLetter = opChar
    , P.reservedNames =
        [ "fn"
        , "struct"
        , "type"
        ]
    , P.reservedOpNames =
        [ ":"
        , "->"
        , ":=" -- not a binop, so not covered below.
        , ".."
        , ","
        ] ++
        binops ++
        (map (++"=") binops) -- +=, -= etc.
    , P.caseSensitive = True
    }
  where
    opChar :: Parser Char
    opChar = P.oneOf "+-*/<>=|&"
    binops = ["+", "-", "*", "/", "&", "|", "^", "<<", ">>"]

lexer = P.makeTokenParser langDef

natural = P.natural lexer
parens = P.parens lexer
reservedOp = P.reservedOp lexer


-- Parser proper

opTable =
    [ [ left "*" Mul
      , left "/" Div
      , left "%" Div
      ]
    , [ left "+" Add
      , left "-" Sub
      ]
    ]
  where
    left txt ast = binary txt ast P.AssocLeft
    binary txt ast assoc =
        P.Infix (reservedOp txt >> (return $ ExBinary ast)) assoc


expr :: Parser Expr
expr = P.buildExpressionParser opTable term

term :: Parser Expr
term = parens expr <|> (ExConst . ConstInt <$> natural)
    -- TODO: we should verify that 'natural' has the syntax we want for
    -- integers -- it accepts the haskell syntax.


-- Tests. Should pull these out into a proper test suite soonish.
tests = and
    [ expr `parses` "4+1" $
        ExBinary Add (ExConst (ConstInt 4)) (ExConst (ConstInt 1))
    , expr `parses` "2 * 7 - 4 / 3" $
        ExBinary Sub
            (ExBinary Mul (ExConst (ConstInt 2)) (ExConst (ConstInt 7)))
            (ExBinary Div (ExConst (ConstInt 4)) (ExConst (ConstInt 3)))
    , expr `parses` "2 * (7 - 4) / 3" $
        ExBinary Div
            (ExBinary Mul
                (ExConst (ConstInt 2))
                (ExBinary Sub
                    (ExConst (ConstInt 7))
                    (ExConst (ConstInt 4))))
            (ExConst (ConstInt 3))
    ]
  where
    parses p text result = P.runParser p () "" text == Right result
    fails p text = case P.runParser p () "" text of
        Right _ -> False
        Left _ -> True
