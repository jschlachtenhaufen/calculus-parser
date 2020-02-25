{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Expressions
import Text.Megaparsec
import Text.Megaparsec.Char

upto :: Char -> Parser String
upto c = (char c *> return []) <|> ((:) <$> anySingle <*> upto c) 
