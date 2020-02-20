{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Expressions
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void

type Parser = Parsec Void String

-- sampleInput = "deriv(x, x * y)"
-- sampleInput2 = "sin(5 + x) / (deriv(x, x^2))"

expr :: Parser Expr
expr =
        Var <$> var
    <|> ConstN <$> decimal
    <|> TermFunc <$> func <*> many expr
    <|> TermOp <$> operator <*> expr <*> expr

-- x, y2
var :: Parser String
var = try $
    do c <- letterChar
       rest <- many digitChar
       return (c:rest) 

-- sin, lambda, 
func :: Parser String
func = try $ 
    do c1 <- letterChar
       c2 <- letterChar
       rest <- many alphaNumChar
       return (c1:c2:rest)

operator :: Parser String
operator = "+" <|> "*" <|> "/" <|> "-"
