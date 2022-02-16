module SmoothPermsTest where

import Test.QuickCheck (quickCheck)

-- import SmoothPermsSlow (perms)
import SmoothPermsSlow.Internal (split)
import Data.List (sort)

{-| 'splitLength' checks if 'split' produces a list of pairs, of the same length
  of the original 'split' argument 'xs'
-}
splitLength :: [Int] -> Bool
splitLength xs = (length . split) xs == length xs


{-| 'splitLengthElems' checks if every pair has a combined length of the same length
  of the original 'split' argument 'xs'
-}
splitLengthElems :: [Int] -> Bool
splitLengthElems xs = splitLengthElem (length xs) (split xs)

splitLengthElem :: Int -> [(Int, [Int])] -> Bool
splitLengthElem _ [] = True
splitLengthElem n (x : xs) = length (fst x : snd x) == n && splitLengthElem n xs


{-| 'splitElems' checks if the elements are the same of the list, for every pair
-}
splitElems :: [Int] -> Bool
splitElems xs = splitElem xs (split xs)

splitElem :: [Int] -> [(Int, [Int])] -> Bool
splitElem _ [] = True
splitElem xs (y : ys) = areEqual (fst y : snd y) xs && splitElem xs ys

{-| 
  https://stackoverflow.com/questions/15319136/how-to-compare-two-lists-in-haskell
-}
areEqual :: [Int] -> [Int] -> Bool
areEqual a b = sort a == sort b

