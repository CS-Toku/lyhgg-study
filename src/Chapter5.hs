module Chapter5(
      multTwoWithNine
    , compareWithHundred
    , compareWithHundred'
    , divideByTen
    , isUpperAlphanum
    , applyTwice
    , zipWith'
    , flip'
    , map'
    , filter'
    , largestDivisible
    , squareOddNumSum
    , collatzchain
    , cnumLongChains
    , listOfFuns
    , flip''
    , foldl'
    , sum'
    , foldr'
    , map''
    , elem'
    , foldl1'
    , maximum'
    , foldr1'
    , reverse'
    , product'
    , filter''
    , last'
    , scanl'
    , scanr'
    , sumSqrt
    , sumSqrt'
) where


multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

multTwoWithNine :: Int -> Int -> Int
multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

divideByTen :: Floating a => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x:map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
    | f x = x:filter' f xs
    | otherwise = filter' f xs

largestDivisible :: Integer -> Integer -> Integer
largestDivisible n d
    | n < d = error "error num"
    | otherwise = head $ filter' (f d) [n, n-1..]
    where
        f d num = num `mod` d == 0

squareOddNumSum :: Integral a => a -> a
squareOddNumSum maxnum = sum $ takeWhile (<maxnum) (filter odd (map (^2) [1..]))

collatzchain :: Integral a => a -> [a]
collatzchain n 
    | n <= 0 = error "Irregular number."
    | n == 1 = [1]
    | odd n = n:collatzchain (3*n+1)
    | even n = n:collatzchain (n`div`2)

cnumLongChains :: Int
cnumLongChains = length $ filter (>15) $ map (length.collatzchain) [1..100]

listOfFuns :: (Enum a, Floating a) => [a->a]
listOfFuns = map (/) [0..]

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ a [] = a
foldr' f a (x:xs) = f x $ foldr' f a xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr' (\x acc -> f x:acc) [] xs

elem' :: Eq a => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "empty list"
foldl1' f (x:xs) = foldl' f x xs

maximum' :: Ord a => [a] -> a
maximum' = foldl1' max

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "empty list"
foldr1' f xs = foldr' f (last xs) (init xs)


reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: Num a => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x:acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ z [] = [z]
scanl' f z (x:xs) = let tz = f z x
                    in  z:(scanl' f tz xs)

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ z [] = [z]
scanr' f z (x:xs) = let zlist = scanr' f z xs
                    in (f x $ head zlist):zlist

scanl1' :: (a -> a -> a) -> [a] -> [a]
scanl1' _ [] = error "empty list"
scanl1' f (x:xs) = scanl' f x xs

scanr1' :: (a -> a -> a) -> [a] -> [a]
scanr1' _ [] = error "empty list"
scanr1' f xs = scanr' f (last xs) (init xs)


sumSqrt :: (Ord a, Enum a, Floating a) => a -> Int
sumSqrt l = compPos 1 $ scanl1' (+) $ map sqrt [1..]
    where
        compPos n (x:xs) = if x <= l 
                            then compPos (n+1) xs
                            else n

sumSqrt' :: (Ord a, Enum a, Floating a) => a -> Int
sumSqrt' l = (+1) $ length.takeWhile (<1000) . scanl1' (+) $ map sqrt [1..]



