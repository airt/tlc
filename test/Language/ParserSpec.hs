{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ParserSpec where

import Control.Category
import Data.Text
import Test.Hspec
import Language.Parser
import Language.Syntax

spec :: Spec
spec = describe "Parser" $ do

  it "Identifier" $ do
    p "λ" `shouldBe` Identifier "λ"
    p "-" `shouldBe` Identifier "-"
    p "equal?" `shouldBe` Identifier "equal?"

  it "Integer" $ do
    p "10" `shouldBe` Integer 10
    p "-10" `shouldBe` Integer (-10)

  it "Floating" $ do
    p "10.01" `shouldBe` Floating 10.01
    p "-10.01" `shouldBe` Floating (-10.01)

  it "Character" $ do
    p "#\\x" `shouldBe` Character 'x'
    p "#\\ " `shouldBe` Character ' '
    p "#\\\\" `shouldBe` Character '\\'
    p "#\\space" `shouldBe` Character ' '
    p "#\\x03BB" `shouldBe` Character 'λ'

  it "Boolean" $ do
    p "#t" `shouldBe` Boolean True
    p "#f" `shouldBe` Boolean False

  it "String" $ do
    p "\"\"" `shouldBe` String ""
    p "\"xx\"" `shouldBe` String "xx"
    p "\"x\\nx\"" `shouldBe` String "x\nx"
    p "\"x\\\"x\"" `shouldBe` String "x\"x"
    p "\"x\\\\x\"" `shouldBe` String "x\\x"

  it "List" $ do
    p "()" `shouldBe` List []
    p "(x)" `shouldBe` List [Identifier "x"]
    p "( x )" `shouldBe` List [Identifier "x"]
    p "(x y)" `shouldBe` List [Identifier "x", Identifier "y"]
    p "(x (y))" `shouldBe` List [Identifier "x", List [Identifier "y"]]
    p "(x 1 0.1 #\\c #t \" \" () #() '())" `shouldBe`
      List [
        Identifier "x",
        Integer 1,
        Floating 0.1,
        Character 'c',
        Boolean True,
        String " ",
        List [],
        Vector [],
        List [Identifier "quote", List []]
      ]

  it "Vector" $ do
    p "#()" `shouldBe` Vector []
    p "#(x)" `shouldBe` Vector [Identifier "x"]
    p "#( x )" `shouldBe` Vector [Identifier "x"]
    p "#(x y)" `shouldBe` Vector [Identifier "x", Identifier "y"]
    p "#(x #(y))" `shouldBe` Vector [Identifier "x", Vector [Identifier "y"]]

  it "Quotation" $ do
    p "'x" `shouldBe` List [Identifier "quote", Identifier "x"]
    p "''x" `shouldBe` List [Identifier "quote", List [Identifier "quote", Identifier "x"]]
    p "'()" `shouldBe` List [Identifier "quote", List []]
    p "'('())" `shouldBe` List [Identifier "quote", List [List [Identifier "quote", List []]]]
    p "' ( ' () )" `shouldBe` List [Identifier "quote", List [List [Identifier "quote", List []]]]

  it "Comment" $ do
    p "x;c\n" `shouldBe` Identifier "x"
    p "x ; c \n" `shouldBe` Identifier "x"
    p "( x ; c \n y )" `shouldBe` List [Identifier "x", Identifier "y"]
    p "( x #| c |# y )" `shouldBe` List [Identifier "x", Identifier "y"]

p :: Text -> Expression
p = parses [] >>> \case
  Left e -> errorWithoutStackTrace $ show e
  Right x -> x
