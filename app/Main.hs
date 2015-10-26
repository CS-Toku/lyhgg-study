
module Main where

import System.Environment (getArgs)
import Text.Read

import Language.Haskell.Interpreter

import Chapter1

main :: IO ()
main = do
        args <- getArgs
        if args == []
            then putStrLn "Invalid Argment."
            else runapp $ readMaybe $ head args


showResult :: String -> IO()
showResult expr = do
    let unqualifieds = ["Prelude", "Chapter1"]
    let qualifieds = []
    result <- runInterpreter $ (setImportsQ $ (zip unqualifieds $ repeat Nothing) ++ qualifieds) >> eval expr
    case result of
        Right x -> putStrLn $ expr ++ " => " ++ x
        Left x -> putStrLn "Error..."

runapp :: Maybe Int -> IO ()
runapp Nothing = putStrLn "Parse error."
runapp (Just 1) = do 

    showResult "1+1"
    showResult "doubleMe 1"
    showResult "2 - 7"
    showResult "2 * 7"
    showResult "2 / 7"
    showResult "2 + 7 * 3"
    showResult "(2 + 7) * 3"

    putStrLn ""
    showResult "True && False"
    showResult "True || False"
    showResult "not True"
    
    putStrLn ""
    showResult "\"hello\" == \"hello\""
    showResult "5 /= 5"

    putStrLn ""
    showResult "succ 8"
    showResult "min 9 11"
    showResult "max 9 11"

    putStrLn ""
    showResult "div 92 10"
    showResult "92 `div` 10"

    putStrLn ""
    showResult "doubleMe 9"
    showResult "doubleUs 2 3"
    showResult "doubleUs_2 2 3"
    showResult "doubleSmallNumber 86"
    showResult "doubleSmallNumber 103"
    showResult "doubleSmallNumber_2 103"
    showResult "conanO_Brien"

    putStrLn ""
    showResult "lostNumbers"
    showResult "[1,2,3,4] ++ [9,10,11,12]"
    showResult "\"hello\" ++ \" \" ++ \"world\""



runapp (Just x) = putStrLn "Not Implemented"


