module Chapter3(
      lucky
    , sayMe
    , fractorial
    , first
    , second
    , third
    , addPair
    , firstLetter
    , bmiTell
    , max'
    , myCompare
    , initials
    , calcBmis
    , cylinder
    , calcBmis'
    , describeList
    , describeList'
    ) where

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, your're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"


fractorial :: Int -> Int
fractorial 0 = 1
fractorial x = x * fractorial (x-1)


addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double) 
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)

first :: (a, b, c) -> a
first (a, _, _) = a
second :: (a, b, c) -> b
second (_, b, _) = b
third :: (a, b, c) -> c
third (_, _, c) = c

addPair :: Num a => [(a, a)] -> [a]
addPair xs = [a+b | (a, b) <- xs]

firstLetter :: String -> String
firstLetter all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight,, you emo, you!"
    | bmi <= normal = "You're supposedly normal."
    | bmi <= fat = "You're fat! Lose so me weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where
        bmi = weight / height^2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: Ord a => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
    | a < b = LT
    | a == b = EQ
    | otherwise = GT

initials :: String -> String -> String
initials firstname lastname = f:'.':l:[]
    where
        (f:_) = firstname
        (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis bmis = [bmi w h | (w, h) <- bmis]
    where
        bmi weight height = weight / height^2

cylinder :: Double -> Double -> Double
cylinder r h = 
    let
        topArea = pi*r^2
        sideArea = 2*pi*r*h
    in
        2*topArea + sideArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' bmis = [bmi | (w, h) <- bmis, let bmi = w / h^2]

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."


describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where
        what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."


