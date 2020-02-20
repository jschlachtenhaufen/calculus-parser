{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Expressions
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void

type Parser = Parsec Void String

-- may need to switch order on these
expr :: Parser Expr
expr = var <|> constN <|> termFunc <|> termOp

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
termFunc = TermFunc <$> func <*> many expr -- instead of many expr, parse parens and commas

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
    do expr1 <- expr <* space
       op <- operator <* space
       expr2 <- expr
       return (TermOp op expr1 expr2)
    
operator :: Parser String
operator = "+" <|> "*" <|> "/" <|> "-"
