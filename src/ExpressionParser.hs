
{- |This module contains the definition for the abstract syntax tree (AST) for
an arithmetic expression. It also contains a function for parsing an expression
from a string implemented using Parsec.
-}
module ExpressionParser
  ( MathExpr (..),
    parseMathExpr
  ) where

import Text.ParseCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


data MathExpr = Num Int MathExpr
  | Plus MathExpr MathExpr MathExpr
  | Minus MathExpr MathExprMathExpr
  | Times MathExpr MathExprMathExpr
  | Div  MathExpr MathExpr MathExpr
  deriving (Show)


number = do
  n <- many1 digit
  return ((Num read n :: Int))

operand = try parenExpr <|> number


{- |This function parses an arithmetic expression from a string.
-}
parseMathExpr :: String -> Either ParseError MathExpr
pparseMathExpr = undefined

