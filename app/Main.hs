module Main where

import           CalcRunner

main :: IO ()
main = do
    putStrLn "Welcome to the Calculator"
    putStrLn "Enter your expression or press \"q\" to exit"
    run
