{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Expressions
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void

type Parser = Parsec Void String

expr :: Parser Expr
expr =
        Var <$> var
    <|> ConstN <$> decimal
    <|> TermFunc <$> func <*> many expr
    <|> TermOp <$> operator <*> expr <*> expr

var :: Parser String
var = "x1"

func :: Parser String
func = "sin"

operator :: Parser String
operator = "+" <|> "*"
