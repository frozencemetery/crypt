{-# LANGUAGE OverloadedStrings #-}
module Tests.Parser where

import Crypt.Ast
import Crypt.Parser

import Data.Monoid                    ((<>))
import Test.Framework                 (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit                     (Test(TestCase, TestList), assertEqual)
import Text.Parsec                    (runParser)

tests = testGroup "Parser Tests" $ hUnitTestToTests $ TestList
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
    , expr `parses` "4 % 2" $
        ExBinary Mod
            (ExConst (ConstInt 4))
            (ExConst (ConstInt 2))
    , expr `parses` "1 & 2 == 3" $
        ExBinary Equals
            (ExBinary BitAnd (ExConst (ConstInt 1)) (ExConst (ConstInt 2)))
            (ExConst (ConstInt 3))
    , expr `parses` "f(x, y, z)" $
        ExApply (ExVar "f") [ExVar "x", ExVar "y", ExVar "z"]
    , expr `parses` "f()" $ ExApply (ExVar "f") []
    , expr `parses` "(2 + 1)(2, 3)" $ ExApply
        (ExBinary Add
            (ExConst (ConstInt 2))
            (ExConst (ConstInt 1)))
        [ ExConst (ConstInt 2)
        , ExConst (ConstInt 3)
        ]
    , typ `parses` "[32]arr" $ TyArray (ExConst (ConstInt 32)) (TyVar "arr")
    , typ `parses` "struct {}" $ TyStruct []
    , typ `parses` "struct { foo: bar }" $ TyStruct [("foo", TyVar "bar")]
    , typ `parses` "struct { foo: bar, baz: quux, }" $ TyStruct
        [ ("foo", TyVar "bar")
        , ("baz", TyVar "quux")
        ]
    , typ `parses` "myTyp" $ TyVar "myTyp"
    , typ `parses` "Foo<bar, baz>" $
        TyApp (TyVar "Foo") [TyArgType $ TyVar "bar", TyArgType $ TyVar "baz"]
    , typ `parses` "u<32>" $ TyApp (TyVar "u") [TyArgNum 32]
    , lval `parses` "W[t]" $ LIndex (ExVar "W") (ExVar "t")
    , lval `parses` "foo" $ LVar "foo"
    , lval `parses` "hello[3 + 2]" $
         LIndex
            (ExVar "hello")
            (ExBinary Add
                (ExConst (ConstInt 3))
                (ExConst (ConstInt 2)))
    , lval `parses` "(4 * 1)[32]" $
        LIndex
            (ExBinary Mul
                (ExConst (ConstInt 4))
                (ExConst (ConstInt 1)))
            (ExConst (ConstInt 32))
    , stmt `parses` "3 + 2;" $
        StmtExpr $
            ExBinary Add
                (ExConst (ConstInt 3))
                (ExConst (ConstInt 2))
    , stmt `parses` "x := 23 + 3 * 21;" $
        StmtAssignDecl
            (LVar "x")
            (ExBinary Add
                (ExConst (ConstInt 23))
                (ExBinary Mul
                    (ExConst (ConstInt 3))
                    (ExConst (ConstInt 21))))
    , stmt `parses` "x = 32;" $
        StmtAssign (LVar "x") (ExConst (ConstInt 32))
    , stmt `parses` "{ 32; x = 1; }" $
        StmtBlock
            [ StmtExpr (ExConst (ConstInt 32))
            , StmtAssign (LVar "x") (ExConst (ConstInt 1))
            ]
    , stmt `parses` "x[7] = 23;" $
        StmtAssign
            (LIndex (ExVar "x") (ExConst (ConstInt 7)))
            (ExConst (ConstInt 23))
    , stmt `parses` "for x := 0..32 { }" $
        StmtFor (LVar "x") (ExConst (ConstInt 0)) (ExConst (ConstInt 32))
            (StmtBlock [])
    , stmt `parses` "for x := 3..7 {\n\tx[i] = 4;\n\t}\n" $
        StmtFor (LVar "x") (ExConst (ConstInt 3)) (ExConst (ConstInt 7))
            (StmtBlock
                [ StmtAssign (LIndex (ExVar "x") (ExVar "i")) (ExConst (ConstInt 4))
                ])
    , constDef `parses` "const Foo: Bar<T> = 32" $
        ( "Foo"
        , DefConst
            (TyApp (TyVar "Bar") [TyArgType $ TyVar "T"])
            (ExConst (ConstInt 32))
        )
    , typeDef `parses` "type Foo = Bar<T>" $
        ("Foo", DefType $ TyApp (TyVar "Bar") [TyArgType $ TyVar "T"])
    , typeDef `parses` "type Foo = struct { x: Bar, y: Baz }" $
        ("Foo", DefType $ TyStruct
            [ ("x", TyVar "Bar")
            , ("y", TyVar "Baz")
            ])
    , fnDef `parses` "fn x() -> int = 4;" $
        ("x", DefFn $ Fn { fnArgs = []
                         , fnReturn = Just (TyVar "int")
                         , fnBody = StmtExpr (ExConst (ConstInt 4))
                         })
    , fnDef `parses` "fn x(y, z : int) = { 2; 7; }" $
        ("x", DefFn $ Fn { fnArgs = [ ArgSpec { argMut = False
                                              , argName = "y"
                                              , argType = TyVar "int"
                                              }
                                    , ArgSpec { argMut = False
                                              , argName = "z"
                                              , argType = TyVar "int"
                                              }
                                    ]
                         , fnReturn = Nothing
                         , fnBody = StmtBlock [ StmtExpr (ExConst (ConstInt 2))
                                              , StmtExpr (ExConst (ConstInt 7))
                                              ]
                         })
    , fnDef `parses` "fn f(x, y : Bar<T>, baz: mut Quux) = 1;" $
        ("f", DefFn $ Fn { fnArgs = [ ArgSpec { argMut = False
                                              , argName = "x"
                                              , argType = TyApp (TyVar "Bar")
                                                                [TyArgType $ TyVar "T"]
                                              }
                                    , ArgSpec { argMut = False
                                              , argName = "y"
                                              , argType = TyApp (TyVar "Bar")
                                                                [TyArgType $ TyVar "T"]
                                              }
                                    , ArgSpec { argMut = True
                                              , argName = "baz"
                                              , argType = TyVar "Quux"
                                              }
                                    ]
                         , fnReturn = Nothing
                         , fnBody = StmtExpr (ExConst (ConstInt 1))
                         })
    , file `parses` "/* this is a file with a comment at the top */\n" $ []
    ]
  where
    parses p text result = TestCase $
        assertEqual
            ("should parse: " <> text)
            (Right result)
            (runParser p () "" text)
    fails p text = TestCase $
        assertEqual
            ("should fail: " <> text)
            (Left ())
            (case runParser p () "" text of
                -- If we get a left, normalize the argument
                -- so we don't have to specify the exact error.
                Left _   -> Left ()
                Right ok -> Right ok)


