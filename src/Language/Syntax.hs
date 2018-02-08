{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Syntax (
  Expression(.., Quote),
  findCharacterByName,
  findNameOfCharacter,
) where

import Data.Map
import Data.String
import Data.Text
import Data.Tuple
import Data.Typeable

data Expression =
  Identifier Text |
  Integer Integer |
  Floating Double |
  Character Char |
  Boolean Bool |
  String Text |
  List [Expression] |
  Vector [Expression]
  deriving (Eq, Show, Typeable)

pattern Quote :: Expression -> Expression
pattern Quote x = List [Identifier "quote", x]

instance IsString Expression where
  fromString = Identifier . pack

findCharacterByName :: Text -> Maybe Char
findCharacterByName = (m !?)
  where
    m = Data.Map.fromList namedCharacters

findNameOfCharacter :: Char -> Maybe Text
findNameOfCharacter = (m !?)
  where
    m = Data.Map.fromList $ swap <$> namedCharacters

namedCharacters :: [(Text, Char)]
namedCharacters =
  [
    ("alarm", '\a'),
    ("backspace", '\b'),
    ("delete", '\DEL'),
    ("escape", '\ESC'),
    ("newline", '\n'),
    ("null", '\NUL'),
    ("return", '\r'),
    ("space", ' '),
    ("tab", '\t')
  ]
