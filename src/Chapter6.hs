module Chapter6(
      numUniques
    ) where

import qualified Data.List as M (nub)

numUniques :: Eq a => [a] -> Int
numUniques = length.M.nub



