module Chapter1(
      doubleMe
    , doubleUs
    , doubleUs_2
    , doubleSmallNumber
    , doubleSmallNumber_2
    , conanO_Brien
    , lostNumbers
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


lostNumbers = [4,8,15,16,23,42]



