module Chapter4(
      maximum'
    , replicate'
    , take'
    , reverse'
    , repeat'
    , zip'
    , elem'
    , quickSort
    ) where


maximum' :: Ord a => [a] -> a
maximum' [] = error "empty list."
maximum' [x] = x
maximum' (x:xs) = max x $ maximum' xs

replicate' :: Int -> a -> [a]
replicate' n elem 
    | n <= 0 = []
    | otherwise = elem:replicate' (n-1) elem

take' :: Int -> [a] -> [a]
take' n  [] = []
take' n (x:xs) 
    | n <= 0 = []
    | otherwise = x:take' (n-1) xs

reverse' ::  [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' ::  a -> [a]
repeat' x = x:repeat' x

zip' ::  [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) 
    | e == x = True
    | otherwise = e `elem'` xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs)= quickSort leList ++ [x] ++ quickSort gtList
    where
        leList = [ l | l <- xs, l <= x ]
        gtList = [ g | g <- xs, g > x ]

