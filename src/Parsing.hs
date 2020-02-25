{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Expressions
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void

type Parser = Parsec Void String

expr = term >>= rest
    where rest e1 = do {p <- space *> operator <* space;
                        e2 <- term;
                        rest (TermOp p e1 e2)} <|> return e1
  
term = ("(" *> expr <* ")") <|> termFunc <|> var <|> constN

-- x, y2
var :: Parser Expr
var = -- try $
    do c <- letterChar
       rest <- many digitChar
       return (Var (c:rest))

-- 5, 78
constN :: Parser Expr
constN = ConstN <$> decimal

-- sin(x, 5*x^2)
termFunc :: Parser Expr
termFunc = TermFunc <$> func <*> (("(" *> exprArgs <* ")") <|> exprArgs)

exprArgs :: Parser [Expr]
exprArgs = try $ sepBy expr (char ',' <* space)

-- sin, lambda, 
func :: Parser String
func = try $ 
    do c1 <- letterChar
       c2 <- letterChar
       rest <- many alphaNumChar
       return (c1:c2:rest)
    
operator :: Parser String
operator = "+" <|> "*" <|> "/" <|> "-" <|> "^"
