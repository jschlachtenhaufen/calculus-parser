{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Expressions
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void

type Parser = Parsec Void String

expr :: Parser Expr
expr = termSecond >>= first

-- first, second, third in order of operation precedence
first :: Expr -> Parser Expr
first e1 = do {op <- space *> addop <* space;
               e2 <- termSecond;
               first (TermOp op e1 e2)} <|> return e1

second :: Expr -> Parser Expr
second e1 = do {op <- mulop;
                e2 <- termThird;
                second (TermOp op e1 e2)} <|> return e1

third :: Expr -> Parser Expr
third e1 = do {op <- powop;
               e2 <- factor;
               third (TermOp op e1 e2)} <|> return e1

termSecond :: Parser Expr      
termSecond = factor >>= second

termThird :: Parser Expr
termThird = factor >>= third

factor :: Parser Expr
factor = ("(" *> expr <* ")") <|> termFunc <|> var <|> constN

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

addop :: Parser String
addop = "+" <|> "-"
    
mulop :: Parser String
mulop =  "*" <|> "/"

powop :: Parser String
powop =  "^"
