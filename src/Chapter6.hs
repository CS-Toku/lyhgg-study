module Chapter6(
      numUniques
    , encode
    , decode
    , digitSum
    , firstTo
    , findKey
    , findKey'
    , findKey''
    , phoneList
    , phoneMap
    , string2digits
    , phoneBookToMap
    ) where

import Data.List
import Data.Char
import qualified Data.Map as Map

numUniques :: Eq a => [a] -> Int
numUniques = length.nub


encode :: Int -> String -> String
encode offset = map $ chr.(+offset).ord

decode :: Int -> String -> String
decode shift = map $ chr.(subtract shift).ord

digitSum :: Int -> Int
digitSum = sum.map digitToInt.show

firstTo :: Int -> Int
firstTo n = case fn of
    (Just num) -> num
    Nothing -> error "none number"
    where
        fn = find (\x -> n == digitSum x) [1..]

findKey :: Eq a => a -> [(a, b)] -> b
findKey key xs = case value of
    (Just value) -> snd value
    Nothing -> error "not found data"
    where
        value = find (\x -> key == fst x) xs

findKey' :: Eq a => a -> [(a, b)] -> b
findKey' key [] = error "not found data"
findKey' key (x:xs)
    | key == fst x = snd x
    | otherwise = findKey' key xs

findKey'' :: Eq a => a -> [(a, b)] -> Maybe b
findKey'' key xs = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing xs

phoneList :: [(String, String)]
phoneList = [
    ("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("bonnie", "452-2929")
    ,("bonnie", "452-2930")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("wendy", "939-8283")
    ,("wendy", "939-8284")
    ,("penny", "853-2492")
    ,("penny", "853-2493")
    ]

phoneMap :: Map.Map String String
phoneMap = Map.fromList phoneList

string2digits :: String -> [Int]
string2digits = map digitToInt.filter isDigit

phoneBookToMap :: Ord k => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(key, number) -> (key, [number])) xs

