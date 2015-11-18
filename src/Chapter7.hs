module Chapter7 (
      Point(..)
    , Shape(..)
    , Shape'(..)
    , area
    , area'
    , nudge
    , baseCircle
    , baseRect
    , Person(..)
    , Vector(..)
    , vplus
    , dotProd
    , vmult
    , Person'(..)
    , mikeD
    , mikeDstr
    , Day(..)
    , LockerState(..)
    , Code
    , LockerMap
    , lockerLookup
    , lockers
    ) where

import Data.Either
import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape' = Circle' Float Float Float |
             Rectangle' Float Float Float Float
        deriving (Show)

data Shape = Circle Point Float |
              Rectangle Point Point
        deriving (Show)

area' :: Shape' -> Float
area' (Circle' _ _ r) = 2 * pi * r
area' (Rectangle' x1 y1 x2 y2) = abs $ (x2 - x1) * (y2 - y1)

area :: Shape -> Float
area (Circle _ r) = 2 * pi * r
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x2 - x1) * (y2 - y1)

nudge :: Shape -> Point -> Shape
nudge (Circle (Point x y) r) (Point dx dy) = Circle (Point (x+dx) (y+dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) (Point dx dy) = Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person {firstName :: String
                    , lastName :: String
                    , age :: Int
                    , height ::Float
                    , phoneNumber ::String
                    , flavor :: String } deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a ->  Vector a -> Vector a
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a ->  Vector a -> a
dotProd (Vector i j k) (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a ->  a -> Vector a
vmult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

data Person' = Person' {firstName' :: String
                    , lastName' :: String
                    , age' :: Int } deriving (Eq, Show, Read)

mikeD = Person' { firstName'="Michael", lastName'="Diamond", age'=43 }
mikeDstr = "Person' { firstName'=\"Michael\", lastName'=\"Diamond\", age'=43}"

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)


type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]
phoneBook :: PhoneBook
phoneBook = 
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8284")
    ,("penny", "853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pn pbook = (name, pn) `elem` pbook

type AssocList k v = [(k, v)]
type IntMap = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker" ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker" ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZD39I"))
    ,(101, (Free, "JAH3I"))
    ,(103, (Free, "IQSA9"))
    ,(105, (Free, "QOTSA"))
    ,(109, (Taken, "893JJ"))
    ,(110, (Taken, "99292"))
    ]





