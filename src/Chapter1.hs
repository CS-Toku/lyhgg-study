module Chapter1(
      doubleMe
    , doubleUs
) where


doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2


