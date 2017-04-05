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
        , "const"
        , "struct"
        , "type"
        , "mut"
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
        , ";"
        ] ++
        binops ++
        (map (++"=") binops) -- +=, -= etc.
    , P.caseSensitive = True
    }
  where
    opChar :: Parser Char
    opChar = P.oneOf "+-*/<>=|&;"
    binops = ["+", "-", "*", "/", "&", "|", "^", "<<", ">>"]

lexer = P.makeTokenParser langDef

angles = P.angles lexer
braces = P.braces lexer
brackets = P.brackets lexer
colon = P.colon lexer
comma = P.comma lexer
identifier = T.pack <$> P.identifier lexer
natural = P.natural lexer
parens = P.parens lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer



-- Parser proper

opTable =
    [ [ left "*" Mul
      , left "/" Div
      , left "%" Div
      , left "<<" ShiftL
      , left ">>" ShiftR
      , left "&" BitAnd
      ]
    , [ left "+" Add
      , left "-" Sub
      , left "|" BitOr
      , left "^" BitXor
      ]
    , [ left "<" Less
      , left ">" Greater
      , left "<=" LtEq
      , left ">=" GtEq
      , left "==" Equals
      , left "!=" NotEquals
      ]
    , [ left "&&" And ]
    , [ left "||" Or ]
    ]
  where
    left txt ast = binary txt ast P.AssocLeft
    binary txt ast assoc =
        P.Infix (reservedOp txt >> (return $ ExBinary ast)) assoc


stmt = P.choice
    [ StmtBlock <$> braces (P.many stmt)
    , P.try $ do
        l <- lval
        P.choice
            [ reservedOp "=" >> StmtAssign l <$> expr
            , reservedOp ":=" >> StmtAssignDecl l <$> expr
            ] <* semi
    , StmtExpr <$> expr <* semi
    ]


expr :: Parser Expr
expr = P.buildExpressionParser opTable term

term :: Parser Expr
term = do
    first <- P.choice
                [ parens expr
                , ExConst . ConstInt <$> natural
                -- TODO: we should verify that 'natural' has the
                -- syntax we want for integers -- it accepts the
                -- haskell syntax.
                , ExVar <$> identifier
                ]
    args <- P.optionMaybe $ parens (expr `P.sepBy` comma)
    return $ case args of
        Nothing -> first
        Just args' -> ExApply first args'

typ :: Parser Type
typ = P.choice
    [ TyArray <$> brackets expr <*> typ
    , reserved "struct" >>
        TyStruct <$> (braces $ field `P.sepEndBy` comma)
    , do
        varName <- TyVar <$> identifier
        args <- P.try $ P.optionMaybe $ angles $ typArg `P.sepEndBy` comma
        return $ case args of
            Just args' -> TyApp varName args'
            Nothing -> varName
    ]
  where
   field = (,) <$> identifier <*> (colon >> typ)
   typArg = P.choice
        [ TyArgNum <$> natural
        , TyArgType <$> typ
        ]

lval :: P.Parser LVal
lval = do
    -- I(zenhack) am unhappy with this.
    tFn <- term
    case tFn of
        ExVar varName -> do
            exArg <- P.optionMaybe (brackets expr)
            return $ case exArg of
                Nothing -> LVar varName
                Just arg -> LIndex tFn arg
        _ -> LIndex tFn <$> brackets expr


file = whiteSpace >> P.many definition <* P.eof

definition = P.choice [typeDef, fnDef, constDef]

typeDef :: P.Parser (T.Text, Def)
typeDef = do
    reserved "type"
    name <- identifier
    reservedOp "="
    ty <- typ
    return (name, DefType ty)

fnDef :: P.Parser (T.Text, Def)
fnDef = do
    reserved "fn"
    name <- identifier
    args <- parens (argSpecs `P.sepBy` comma)
    returnType <- P.optionMaybe $ do
        reservedOp "->"
        typ
    reservedOp "="
    body <- stmt
    return (name, DefFn $ Fn { fnArgs = concat args
                             , fnReturn = returnType
                             , fnBody = body
                             })
  where
    argSpecs = do
        argNames <- identifier `P.sepBy` comma
        colon
        mut <- (/=Nothing) <$> P.optionMaybe (reserved "mut")
        ty <- typ
        return $ map (\name -> ArgSpec { argMut = mut
                                       , argName = name
                                       , argType = ty
                                       })
                     argNames

constDef :: P.Parser (T.Text, Def)
constDef = do
    reserved "const"
    name <- identifier
    colon
    ty <- typ
    reservedOp "="
    value <- expr
    return (name, DefConst ty value)
