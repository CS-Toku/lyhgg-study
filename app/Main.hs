{-# OPTIONS_GHC -cpp -pgmPgcc -optP-E #-}

#define SHOW_RESULT(a) (showResult #a (a))

module Main where

import System.Environment (getArgs)
import Text.Read

import Chapter1

main :: IO ()
main = do
        args <- getArgs
        if args == []
            then putStrLn "Invalid Argment."
            else runapp $ readMaybe $ head args


showResult :: Show a => String -> a -> IO()
showResult expr value = do
    putStr $ expr ++ " => "
    print value


runapp :: Maybe Int -> IO ()
runapp Nothing = putStrLn "Parse error."
runapp (Just 1) = do 
    SHOW_RESULT(2 + 7)
    SHOW_RESULT(2 - 7)
    SHOW_RESULT(2 * 7)
    SHOW_RESULT(2 / 7)
    SHOW_RESULT(2 + 7 * 3)
    SHOW_RESULT((2 + 7) * 3)

    putStrLn ""
    print $ True && False
    print $ True || False
    print $ not True
    
    putStrLn ""
    print $ "hello" == "hello"
    print $ 5 /= 5

    putStrLn ""
    print $ succ 8
    print $ min 9 11
    print $ max 9 11

    putStrLn ""
    print $ div 92 10
    print $ 92 `div` 10

    putStrLn ""
    print $ doubleMe 9
    print $ doubleUs 2 3



runapp (Just x) = putStrLn "Not Implemented"

