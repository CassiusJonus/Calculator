module CalcRunner
    ( run
    ) where

import           CalculatorUtils

import           Data.Char                      ( toLower )
import           Text.Parsec                    ( ParseError )


run :: IO ()
run = do
    expression <- getLine
    if map toLower expression == "q"
        then do
            putStrLn "Exiting Calculator"
            return ()
        else do
            let expr = parseMathExpr expression
            case expr of
                Left e -> do
                    putStrLn $ "Invalid expression."
                    run
                Right mathExpr -> do
                    putStrLn $ show (evaluate mathExpr)
                    run

