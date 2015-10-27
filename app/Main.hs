
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

showGhcError :: [GhcError] -> IO()
showGhcError [] = putStrLn ""
showGhcError (GhcError err:errlist) = do
    putStrLn err
    showGhcError errlist

showResult :: String -> IO()
showResult expr = do
    let unqualifieds = ["Prelude", "Chapter1"]
    let qualifieds = []
    result <- runInterpreter $ (setImportsQ $ (zip unqualifieds $ repeat Nothing) ++ qualifieds) >> eval expr
    case result of
        Right x -> putStrLn $ expr ++ " => " ++ x
        Left e -> case e of
            GhcException p ->  putStrLn $ "Error(GhcException) => " ++ p
            UnknownError p ->  putStrLn $ "Error(UnknownError) => " ++ p
            WontCompile p ->  do
                putStrLn "Error(WontCompile) =>"
                showGhcError p
            NotAllowed p ->  putStrLn $ "Error(NotAllowed) => " ++ p
        


runapp :: Maybe Int -> IO ()
runapp Nothing = putStrLn "Parse error."
runapp (Just 1) = do 
{-
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
    showResult "['w','o'] ++ ['o','t']"
    showResult "'A':\" SMALL CAT\""
    showResult "'A':\" SMALL CAT\""
    showResult "[1,2,3,4] ++ [5]"

    putStrLn ""
    showResult "\"Steve Buscemi\" !! 6 "
    showResult "nestingList"
    showResult "nestingList ++ [[1,1,1,1]]"
    showResult "[6,6,6]:nestingList"

    putStrLn ""
    showResult "[3,2,1] > [2,1,0]"
    showResult "[3,2,1] > [2,10,100]"
    showResult "[3,4,2] < [3,4,3]"
    showResult "[3,4,2] > [2,4]"
    showResult "[3,4,2] == [3,4,2]"

    putStrLn ""
    showResult "head' [5,4,3,2,1]"
    showResult "tail' [5,4,3,2,1]"
    showResult "last' [5,4,3,2,1]"
    showResult "init' [5,4,3,2,1]"
    showResult "length' [5,4,3,2,1]"
    showResult "null' [5,4,3,2,1]"
    showResult "null' []"
    showResult "reverse' [5,4,3,2,1]"
    showResult "take' 3 [5,4,3,2,1]"
    showResult "drop' 3 [5,4,3,2,1]"
    showResult "maximum' [5,4,3,2,1]"
    showResult "minimum' [5,4,3,2,1]"
    showResult "sum' [5,4,3,2,1]"
    showResult "product' [5,4,3,2,1]"
    showResult "elem' 4 [5,4,3,2,1]"
    showResult "elem' 6 [5,4,3,2,1]"
-}
    putStrLn ""
    

    showResult "error \"test\""

runapp (Just x) = putStrLn "Not Implemented"


