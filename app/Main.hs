{-# OPTIONS_GHC -cpp -pgmPgcc -optP-E #-}

#define SHOW_RESULT(a) (showResult #a (a))

module Main where

import System.Environment (getArgs)
import Text.Read

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr

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
    SHOW_RESULT(True && False)
    SHOW_RESULT(True || False)
    SHOW_RESULT(not True)
    
    putStrLn ""
    SHOW_RESULT("hello" == "hello")
    SHOW_RESULT(5 /= 5)

    putStrLn ""
    SHOW_RESULT(succ 8)
    SHOW_RESULT(min 9 11)
    SHOW_RESULT(max 9 11)

    putStrLn ""
    SHOW_RESULT(div 92 10)
    SHOW_RESULT(92 `div` 10)

    putStrLn ""
    SHOW_RESULT(doubleMe 9)
    SHOW_RESULT(doubleUs 2 3)
    SHOW_RESULT(doubleUs_2 2 3)
    SHOW_RESULT(doubleSmallNumber 86)
    SHOW_RESULT(doubleSmallNumber 103)
    SHOW_RESULT(doubleSmallNumber_2 103)
    SHOW_RESULT(conanO_Brien)

    putStrLn ""
    SHOW_RESULT(lostNumbers)
    SHOW_RESULT("hello" ++ " " ++ "world")


runapp (Just x) = putStrLn "Not Implemented"

