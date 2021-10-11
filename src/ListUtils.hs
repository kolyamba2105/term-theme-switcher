module ListUtils (ListUtils.last) where

import           Data.List

last :: [a] -> Maybe a
last [] = Nothing
last xs = Just $ Data.List.last xs
