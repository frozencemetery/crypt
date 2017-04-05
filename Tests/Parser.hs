{-# LANGUAGE OverloadedStrings #-}
module Tests.Parser where

import Crypt.Ast
import Crypt.Parser

import Data.Monoid ((<>))
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework (testGroup)
import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Text.Parsec (runParser)

-- Tests. Should pull these out into a proper test suite soonish.
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
    , expr `parses` "1 & 2 == 3" $
        ExBinary Equals
            (ExBinary BitAnd (ExConst (ConstInt 1)) (ExConst (ConstInt 2)))
            (ExConst (ConstInt 3))
    , typ `parses` "[32]arr" $ TyArray (ExConst (ConstInt 32)) (TyVar "arr")
    , typ `parses` "struct {}" $ TyStruct []
    , typ `parses` "struct { foo: bar }" $ TyStruct [("foo", TyVar "bar")]
    , typ `parses` "struct { foo: bar, baz: quux, }" $ TyStruct
        [ ("foo", TyVar "bar")
        , ("baz", TyVar "quux")
        ]
    , typ `parses` "myTyp" $ TyVar "myTyp"
    , typ `parses` "Foo<bar, baz>" $
        TyApp (TyVar "Foo") [TyVar "bar", TyVar "baz"]
    ]
  where
    parses p text result = TestCase $
        assertEqual
            ("should parse: " <> text)
            (runParser p () "" text)
            (Right result)
    fails p text = TestCase $
        assertEqual
            ("should fail: " <> text)
            (case runParser p () "" text of
                -- If we get a left, normalize the argument
                -- so we don't have to specify the exact error.
                Left _ -> Left ()
                Right ok -> Right ok)
            (Left ())


