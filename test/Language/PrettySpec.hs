{-# LANGUAGE OverloadedStrings #-}

module Language.PrettySpec where

import Test.Hspec
import Language.Pretty
import Language.Syntax

spec :: Spec
spec = describe "Pretty" $ do

  it "Identifier" $ do
    pretty (Identifier "λ") `shouldBe` "λ"
    pretty (Identifier "-") `shouldBe` "-"
    pretty (Identifier "equal?") `shouldBe` "equal?"

  it "Integer" $ do
    pretty (Integer 10) `shouldBe` "10"
    pretty (Integer (-10)) `shouldBe` "-10"

  it "Floating" $ do
    pretty (Floating 10.01) `shouldBe` "10.01"
    pretty (Floating (-10.01)) `shouldBe` "-10.01"

  it "Character" $ do
    pretty (Character 'x') `shouldBe` "#\\x"
    pretty (Character '\'') `shouldBe` "#\\'"
    pretty (Character '\\') `shouldBe` "#\\\\"
    pretty (Character ' ') `shouldBe` "#\\space"
    pretty (Character 'λ') `shouldBe` "#\\x3bb"

  it "Boolean" $ do
    pretty (Boolean True) `shouldBe` "#t"
    pretty (Boolean False) `shouldBe` "#f"

  it "String" $ do
    pretty (String "") `shouldBe` "\"\""
    pretty (String "xx") `shouldBe` "\"xx\""
    pretty (String "x\nx") `shouldBe` "\"x\\nx\""
    pretty (String "x\"x") `shouldBe` "\"x\\\"x\""
    pretty (String "x\\x") `shouldBe` "\"x\\\\x\""

  it "List" $ do
    pretty (List []) `shouldBe` "()"
    pretty (List [Identifier "x"]) `shouldBe` "(x)"
    pretty (List [Identifier "x", Identifier "y"]) `shouldBe` "(x y)"
    pretty (List [Identifier "x", List [Identifier "y"]]) `shouldBe` "(x (y))"
    "(x 1 0.1 #\\c #t \" \" () #() '())" `shouldBe`
      pretty (List [
        Identifier "x",
        Integer 1,
        Floating 0.1,
        Character 'c',
        Boolean True,
        String " ",
        List [],
        Vector [],
        List [Identifier "quote", List []]
      ])

  it "Vector" $ do
    pretty (Vector []) `shouldBe` "#()"
    pretty (Vector [Identifier "x"]) `shouldBe` "#(x)"
    pretty (Vector [Identifier "x", Identifier "y"]) `shouldBe` "#(x y)"
    pretty (Vector [Identifier "x", Vector [Identifier "y"]]) `shouldBe` "#(x #(y))"

  it "Quotation" $ do
    pretty (List [Identifier "quote", Identifier "x"]) `shouldBe` "'x"
    pretty (List [Identifier "quote", List [Identifier "quote", Identifier "x"]]) `shouldBe` "''x"
    pretty (List [Identifier "quote", List [List [Identifier "quote", List []]]]) `shouldBe` "'('())"
