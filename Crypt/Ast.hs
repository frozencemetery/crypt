module Crypt.Ast where

import Data.Text (Text)

data File
    = File [(Text, Def)]
    deriving(Show, Eq)

data Def
    = DefType Type
    | DefFn Fn
    | DefConst Type Expr
    deriving(Show, Eq)

data ArgSpec = ArgSpec
    { argMut  :: Bool
    , argName :: Text
    , argType :: Type
    } deriving(Show, Eq)

data Type
    = TyVar Text
    | TyInt Int
    | TyWord Int
    | TyArray Int Type
    | TyStruct [(Text, Type)]
    | TyApp Type [Type]
    deriving(Show, Eq)

data Fn = Fn
    { fnArgs   :: [ArgSpec]
    , fnReturn :: Type
    , fnBody   :: Stmt
    } deriving(Show, Eq)


data Stmt
    = StmtExpr Expr
    | StmtBlock [Stmt]
    | StmtAssign LVal Expr
    | StmtAssignDecl Text Expr
    deriving(Show, Eq)

data Expr
    = ExConst Const
    | ExArray [Expr]
    | ExBinary BinOp Expr Expr
    | ExApply Expr [Expr]
    | ExVar Text
    | ExHasType Expr Type
    | ExGet Expr Text
    | ExIndex Expr Expr
    deriving(Show, Eq)

data Const
    = ConstInt Integer
    deriving(Show, Eq)

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | BitAnd
    | BitOr
    | BitXor
    | BitNot
    | ShiftL
    | ShiftR
    deriving(Show, Eq)

data LVal
    = LVar Text
    | LIndex Expr Expr
    deriving(Show, Eq)