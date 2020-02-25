{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c) 

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"

spaceAround :: Parser a -> Parser a
spaceAround p = space *> p <* space
