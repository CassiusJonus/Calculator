
{- |This module contains the definition for the abstract syntax tree (AST) for
an arithmetic expression. It also contains a function for parsing an expression
from a string implemented using Parsec.
-}
module ExpressionParser
  ( MathExpr (..),
    parseMathExpr
  ) where

import Text.ParseCombinators.Parsec
import Text.Parser.Expression
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

data MathExpr where
  Number :: Int -> MathExpr
  Plus :: MathExpr -> MathExpr -> MathExpr
  Minus :: MathExpr -> MathExpr -> MathExpr
  Times :: MathExpr -> MathExpr -> MathExpr
  Div :: MathExpr -> MathExpr -> MathExpr
  deriving (Show)

{- |This function parses an arithmetic expression from a string.
-}
parseMathExpr :: String -> Either ParseError MathExpr
pparseMathExpr = undefined

