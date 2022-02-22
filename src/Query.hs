module Query (doQuery, Query (QP, QNot, QAnd, QOr)) where

{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor.Identity
import           Data.Serialize.Text ()
import           Data.Text (Text, pack)
import           Text.Parsec
import           Text.Parsec.Text (Parser)
import           Text.Parsec.Token
import qualified Text.Parsec.Token as Token

lan :: GenLanguageDef Text u Data.Functor.Identity.Identity
lan =
  LanguageDef
    { identStart = alphaNum,
      identLetter = alphaNum <|> oneOf "'",
      reservedOpNames = ["not", "and", "or"],
      caseSensitive = False,
      commentStart = "{-",
      commentEnd = "-}", commentLine = "--", nestedComments = False,
      opStart        = opLetter lan,
      opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~",
      reservedNames  = []
    }

lexer :: GenTokenParser Text u Identity
lexer = Token.makeTokenParser lan

tidentifier :: ParsecT Text u Identity String
tidentifier = Token.identifier lexer

treservedOp :: String -> ParsecT Text u Identity ()
treservedOp = Token.reservedOp lexer

tparens :: ParsecT Text u Identity a -> ParsecT Text u Identity a
tparens = Token.parens lexer

data Query
  = QP Text
  | QNot Query
  | QAnd Query Query
  | QOr Query Query
  deriving (Show)

par :: ParsecT Text () Identity Query
par = pand

pand :: Parser Query
pand = por `chainl1` (do treservedOp "and"; return (QAnd))

por :: Parser Query
por = term `chainl1` (do treservedOp "or"; return (QOr))

term :: ParsecT Text () Identity Query
term =
  (treservedOp "not" >> par >>= \x -> return (QNot x))
    <|> tparens par
    <|> (tidentifier >>= \x -> return (QP (pack x)))

doQuery :: Text -> Either ParseError Query
doQuery t = parse par "" t
