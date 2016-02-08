module Lexer where

import           Text.Parsec
import           Text.Parsec.Language (haskellStyle)
import           Text.Parsec.String
import qualified Text.Parsec.Token    as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = ["True", "False","derive"]
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer
