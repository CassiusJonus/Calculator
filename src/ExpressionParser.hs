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
  spaces
  return ((Num read n :: Int))

operand = try parenExpr <|> number

parenExpr = do
  char '('
  e <- expr
  char ')'
  return e



