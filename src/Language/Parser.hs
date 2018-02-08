{-# LANGUAGE OverloadedStrings #-}

module Language.Parser (
  parse,
  parses,
) where

import Data.Char
import Data.Functor.Identity
import Data.Maybe
import Data.Text
import Numeric
import Text.Parsec hiding (parse, string)
import Text.Parsec.Text
import Language.Syntax
import qualified Text.Parsec.Token as Token

parse :: SourceName -> Text -> Either ParseError [Expression]
parse = runParser (completes expressions) ()

parses :: SourceName -> Text -> Either ParseError Expression
parses = runParser (completes expression) ()

expression :: Parser Expression
expression =
  vector <|>
  list <|>
  string <|>
  boolean <|>
  character <|>
  floating <|>
  integer <|>
  identifier <|>
  quotation <?>
  "expression"

expressions :: Parser [Expression]
expressions = many $ lexeme expression

quotation :: Parser Expression
quotation = Quote <$> (lexeme (char '\'') *> expression)

identifier :: Parser Expression
identifier = Identifier . pack <$> Token.identifier lexer

integer :: Parser Expression
integer = Integer <$> try (sign <*> Token.natural lexer)

floating :: Parser Expression
floating = Floating <$> try (sign <*> Token.float lexer)

character :: Parser Expression
character = Character <$> (try prefix *> contents)
  where
    prefix = char '#' *> char '\\'
    contents = try single <|> scalar <|> named
    single = anyChar <* notFollowedBy alphaNum
    scalar = maybe (fail "invalid hex scalar") return . rx =<< char 'x' *> many hexDigit
    named = maybe (fail "unsupported character name") return . rn =<< Token.identifier lexer
    rx = ((chr . fst) <$>) . listToMaybe . readHex
    rn = findCharacterByName . pack

boolean :: Parser Expression
boolean = Boolean . (== 't') <$> try (char '#' *> oneOf "tf")

string :: Parser Expression
string = String . pack <$> Token.stringLiteral lexer

list :: Parser Expression
list = List <$> parens expressions

vector :: Parser Expression
vector = Vector <$> try (char '#' *> parens expressions)

sign :: Num n => Parser (n -> n)
sign = (negate <$ char '-') <|> (id <$ char '+') <|> return id

completes :: Parser a -> Parser a
completes = between (Token.whiteSpace lexer) eof . lexeme

parens :: Parser a -> Parser a
parens p = Token.parens lexer p <|> Token.brackets lexer p

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

symbol :: Parser Char
symbol = oneOf "!$%&*+-./:<=>?@^_~"

lexer :: Token.GenTokenParser Text () Identity
lexer = Token.makeTokenParser Token.LanguageDef {
  Token.commentStart = "#|",
  Token.commentEnd = "|#",
  Token.commentLine = ";",
  Token.nestedComments = True,
  Token.identStart = letter <|> symbol,
  Token.identLetter = letter <|> digit <|> symbol,
  Token.opStart = unexpected "operator",
  Token.opLetter = unexpected "operator",
  Token.reservedNames = [],
  Token.reservedOpNames = [],
  Token.caseSensitive = True
}
