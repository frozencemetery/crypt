{-# LANGUAGE OverloadedStrings #-}
module Crypt.Parser where

import qualified Data.Text as T
import Control.Applicative ((<|>))
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
        , "<"
        , ">"
        , "<="
        , ">="
        , "=="
        , "!="
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

braces = P.braces lexer
brackets = P.brackets lexer
identifier = T.pack <$> P.identifier lexer
natural = P.natural lexer
parens = P.parens lexer
reserved = P.reserved lexer
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

typ :: Parser Type
typ = P.choice
    [ TyArray <$> brackets expr <*> typ
    , reserved "struct" >>
        TyStruct <$> (braces $ field `P.sepEndBy` reservedOp ",")
    , TyVar <$> P.try (identifier <* P.notFollowedBy (reservedOp "<"))
    ]
  where
   field = (,) <$> identifier <*> (reservedOp ":" >> typ)
