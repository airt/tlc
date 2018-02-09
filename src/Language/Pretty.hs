{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Pretty (
  pprint,
  pretty,
) where

import Data.Char
import Data.Text
import Data.Text.IO
import Numeric
import Text.PrettyPrint
import Language.Syntax

pprint :: Expression -> IO ()
pprint = Data.Text.IO.putStrLn . pretty

pretty :: Expression -> Text
pretty = pack . render . codify

codify :: Expression -> Doc
codify = \case
  Identifier x -> text $ unpack x
  Integer n -> integer n
  Floating n -> double n
  Character c -> "#\\" <> escape c
  Boolean b -> if b then "#t" else "#f"
  String s -> text $ show s
  Quote x -> char '\'' <> codify x
  List xs -> parens . hsep $ codify <$> xs
  Vector xs -> char '#' <> codify (List xs)

escape :: Char -> Doc
escape = \case
  (findNameOfCharacter -> Just n) -> text $ unpack n
  c | c > '\x7F' -> char 'x' <> text (showHex (ord c) [])
  c -> char c
