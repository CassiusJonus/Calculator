module ExpressionParser
  ( MathExpr(..)
  , parseMathExpr
  ) where

import           Data.Functor
import           Data.Functor.Identity          ( Identity )
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr
import           Text.Parsec.Prim
import           Text.Parsec.String

data MathExpr = Num Int
  | Negate MathExpr
  | Plus MathExpr MathExpr
  | Minus MathExpr MathExpr
  | Times MathExpr MathExpr
  | Div  MathExpr MathExpr
  deriving (Show)



number :: ParsecT String () Identity MathExpr
number = do
  n <- many1 digit
  spaces
  return ((Num (read n :: Int)))


operand = try parenExpr <|> number

parenExpr = between (char '(') (char ')') mathExpr <?> "simple expression"



opTable =
  [ [prefix "-" Negate]
  , [binary "*" Times AssocLeft, binary "/" Div AssocLeft]
  , [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft]
  ]
prefix
  :: (Monad m)
  => String
  -> (MathExpr -> MathExpr)
  -> Operator String s m MathExpr
prefix name func = Prefix
  (do
    string name
    spaces
    return func
  )

binary
  :: (Monad m)
  => String
  -> (MathExpr -> MathExpr -> MathExpr)
  -> Assoc
  -> Operator String s m MathExpr
binary name func = Infix
  (do
    string name
    return func
  )

fullExpr = do
  exp <- expr
  spaces
  return exp
  where expr = buildExpressionParser opTable operand <?> "math expression"


mathExpr = do
  result <- fullExpr
  eof
  return result


parseMathExpr = parse mathExpr "Invalid expression"
