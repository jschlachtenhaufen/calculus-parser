{-# LANGUAGE OverloadedStrings #-}
module Expressions where

import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void

data Expr = 
      Var String -- x,y
    | ConstN Double -- 3,5,2
    | TermFunc String [Expr] -- sin, deriv w/ args, lambda
    | TermOp String Expr Expr deriving (Eq, Show) -- -,+,*,/ with args

expr :: Parser Expr
expr =  factor >>= third >>= second >>= first

factor :: Parser Expr
factor = term
-- factor = ("-"((TermOp "*" (ConstN "-1")) <$> "-" *> term) <|> term
-- factor = do {_ <- "-";
--             ((TermOp "*" (ConstN "-1")) <$> term)} <|> return term
  
term :: Parser Expr
term = spaceAround (parens expr <|> termFunc <|> var <|> constN)

-- first, second, third in order of operation precedence
first :: Expr -> Parser Expr
first e1 = do {op <- addop;
               e2 <- (factor >>= third >>= second);
               first (TermOp op e1 e2)} <|> return e1

second :: Expr -> Parser Expr
second e1 = do {op <- mulop;
                e2 <- (factor >>= third);
                second (TermOp op e1 e2)} <|> return e1

third :: Expr -> Parser Expr
third e1 = do {op <- powop;
               e2 <- factor;
               third (TermOp op e1 e2)} <|> return e1

-- x, y2
var :: Parser Expr
var = do c <- letterChar
         rest <- many digitChar
         return (Var (c:rest))

-- 5, 78
constN :: Parser Expr
constN = ConstN <$> decimal

-- sin(x, 5*x^2)
termFunc :: Parser Expr
termFunc = TermFunc <$> func <*> (parens exprArgs <|> exprArgs)

-- sin, lambda, 
func :: Parser String
func = try $ 
    do c1 <- letterChar
       c2 <- letterChar
       rest <- many alphaNumChar
       return (c1:c2:rest)

-- comma seperated exprs inside the brackets of a function
exprArgs :: Parser [Expr]
exprArgs = try $ sepBy expr (char ',' <* space)

addop :: Parser String
addop = "+" <|> "-"
    
mulop :: Parser String
mulop =  "*" <|> "/"

powop :: Parser String
powop =  "^"