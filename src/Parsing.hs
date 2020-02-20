{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Expressions
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void

type Parser = Parsec Void String

expr :: Parser Expr
expr = ("(" *> exprs <* ")") <|> exprs
  where exprs = termOp <|> termFunc <|> var <|> constN

-- x, y2
var :: Parser Expr
var = try $
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

-- x+y, 124 * lambda
termOp :: Parser Expr
termOp = try $
    do expr1 <- (constN <|> termFunc <|> var) <* space -- how do we handle (x+y)+z? The recursion keeps breaking it. "expr1 <- expr <*space" doesn't work
       op <- operator <* space
       expr2 <- expr
       return (TermOp op expr1 expr2)
    
operator :: Parser String
operator = "+" <|> "*" <|> "/" <|> "-" <|> "^"
