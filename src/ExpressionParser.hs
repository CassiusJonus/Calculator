module ExpressionParser
  ( MathExpr(..)
  , parseMathExpr
  ) where

import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr
import           Text.Parsec.Prim

data MathExpr = Num Int
  | Negate MathExpr
  | Plus MathExpr MathExpr
  | Minus MathExpr MathExpr
  | Times MathExpr MathExpr
  | Div  MathExpr MathExpr
  deriving (Show)


number = do
  n <- many1 digit
  spaces
  return ((Num (read n :: Int)))

operand = try parenExpr <|> number

parenExpr = do
  char '('
  e <- expr
  char ')'
  return e

opTable =
  [ [prefix "-" Negate]
  , [binary "*" Times, binary "/" Div]
  , [binary "=" Plus, binary "-" Minus]
  ]


parseMathExpr = undefined
