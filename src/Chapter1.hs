module Chapter1(
      doubleMe
    , doubleUs
    , doubleUs_2
    , doubleSmallNumber
    , doubleSmallNumber_2
    , conanO_Brien
    , lostNumbers
    , nestingList
    , head'
    , tail'
    , last'
    , init'
    , length'
    , null'
    , reverse'
    , take'
    , drop'
    , maximum'
    , minimum'
    , sum'
    , product'
    , elem'
    , cycle'
    , repeat'
    , replicate'
    , boomBangs
    , nouns
    , adjectives
    , length''
    , removeNonUppercase
    , nestNumberList
) where


doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2

doubleUs_2 :: Num a => a -> a -> a
doubleUs_2 x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x <= 100
    then doubleMe x
    else x

doubleSmallNumber_2 :: (Num a, Ord a) => a -> a
doubleSmallNumber_2 x = 1 + if x <= 100
    then doubleMe x
    else x

conanO_Brien :: String
conanO_Brien = "It's a-me, Conan O'Brien!"


lostNumbers :: Num a => [a]
lostNumbers = [4,8,15,16,23,42]

nestingList :: Num a => [[a]]
nestingList = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]

head' ::  [a] -> a
head' [] = error "empty list."
head' (x:_) = x

tail' ::  [a] -> [a]
tail' [] = error "empty list."
tail' (_:xs) = xs

last' ::  [a] -> a
last' [] = error "empty list."
last' (x:[]) = x
last' (_:xs) = last' xs

init' ::  [a] -> [a]
init' [] = error "empty list."
init' (x:[]) = []
init' (x:xs) = x:init' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

null' :: [a] -> Bool
null' [] = True
null' _ = False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' t (x:xs) = x:(take' (t-1) xs)

drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' _ [] = []
drop' t (x:xs) = drop' (t-1) xs

maximum' :: Ord a => [a] -> a
maximum' [] = error "empty list."
maximum' (x:[]) = x
maximum' (x:xs) = if x < xmax
    then xmax
    else x
    where
        xmax = maximum' xs

minimum' :: Ord a => [a] -> a
minimum' [] = error "empty list."
minimum' (x:[]) = x
minimum' (x:xs) = if x > xmin
    then xmin
    else x
    where
        xmin = minimum' xs

sum' :: Num a => [a] -> a
sum' [] = error "empty list."
sum' (x:[]) = x
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = error "empty list."
product' (x:[]) = x
product' (x:xs) = x * product' xs

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = if e == x
    then True
    else elem' e xs

cycle' :: [a] -> [a]
cycle' [] = error "empty list."
cycle' x = x ++ cycle' x

repeat' :: a -> [a]
repeat' x = x:repeat' x

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n-1) x

boomBangs :: [Int] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

nouns :: [String]
nouns = ["hobo", "frog", "pope"]
adjectives :: [String]
adjectives = ["lazy", "grouchy", "scheming"]

length'' :: [a] -> Int
length'' xs = sum [ 1 | _ <- xs ]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

nestNumberList = [  
                    [1,2,3,4,5],
                    [2,3,4,5,6],
                    [3,4,5,6,7]
                 ]




