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


resultShow :: Show a => String -> a -> IO()
resultShow expr result = do
    putStr expr
    putStr " => "
    print result


runapp :: Maybe Int -> IO ()
runapp Nothing = putStrLn "Parse error."
runapp (Just 1) = do 
    resultShow "2+7" $ 2+7
    resultShow "2-7" $ 2-7
    resultShow "2*7" $ 2*7
    resultShow "2/7" $ 2/7
    resultShow "2+7*3" $ 2+7*3
    resultShow "(2+7)*3" $ (2+7)*3

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

