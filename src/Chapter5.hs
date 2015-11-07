module Chapter5(
      multTwoWithNine
    , compareWithHundred
    , compareWithHundred'
    , divideByTen
    , isUpperAlphanum
    , applyTwice
    , zipWith'
    , map'
    , filter'
    , largestDivisible
    , squareOddNumSum
    , collatzchain
    , cnumLongChains
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

