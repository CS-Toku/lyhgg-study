module Chapter6(
      numUniques
    , wordNums
    , isIn
    , isIn'
    , encode
    ) where

import Data.List
import Data.Char

numUniques :: Eq a => [a] -> Int
numUniques = length.nub

wordNums ::  String -> [(String, Int)]
wordNums = map (\xs -> (head xs, length xs)).group.sort.words

isIn :: Eq a => [a] -> [a] -> Bool
isIn needle haystack = foldr1 (||).map (isPrefixOf needle).tails $ haystack

isIn' :: Eq a => [a] -> [a] -> Bool
isIn' needle = any (isPrefixOf needle).tails

encode :: Int -> String -> String
encode offset = map $ chr.(+offset).ord


