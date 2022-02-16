module SmoothPermsTest where

import SmoothPermsSlow
import SmoothPermsSlow.Internal (split, smooth)
import Data.List (sort)

-- >Internal.hs
-- >>split

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


-- >>smooth
-- doesn't need testing


-- >SmoothPermsSlow.hs
-- >>perms

{-| 'permsLength' checks if the number of permutations is right
  the right number of permutations of a set of size n is n!
-}
permsLength :: [Int] -> Bool
permsLength xs = permsCorrectLength xs == (length . perms) xs

permsCorrectLength :: [Int] -> Int
permsCorrectLength = factorial . length
  
factorial :: Int -> Int
factorial n = product [1..n]

{-| 'permsLengthElems' checks if all perms have the right length
  the right length is the length of 'xs'
-}
permsLengthElems :: [Int] -> Bool
permsLengthElems xs = permsLengthElem (length xs) (perms xs)

permsLengthElem :: Int -> [[Int]] -> Bool
permsLengthElem _ [] = True
permsLengthElem n (x : xs) = length x == n && permsLengthElem n xs

{-| 'permsElems' checks if all perms have all the elements
  compares each permutation with the original list 'xs'
-}
permsElems :: [Int] -> Bool
permsElems xs = permsElem xs (perms xs)

permsElem :: [Int] -> [[Int]] -> Bool
permsElem _ [] = True
permsElem xs (y : ys) = areEqual xs y && permsElem xs ys

-- {-| 'permsUnique' checks if all the perms are unique
--   EDIT: how to check for  'perms [-2, -2]'?
-- -}
-- permsUnique :: [Int] -> Bool
-- permsUnique = allDifferent . perms

{-|
  https://stackoverflow.com/questions/31036474/haskell-checking-if-all-list-elements-are-unique
-}
allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

-- >> smoothPerms

{-| 'smoothPermsAreSmooth' checks if all the permutations of 'smoothPerms' are smooth;
-}
smoothPermsAreSmooth :: Int -> [Int] -> Bool
smoothPermsAreSmooth n xs = smoothPermIsSmooth n (smoothPerms n xs)

smoothPermIsSmooth :: Int -> [[Int]] -> Bool
smoothPermIsSmooth _ [] = True
smoothPermIsSmooth n (x : xs) = smooth n x && smoothPermIsSmooth n xs

{-| 'smoothPermsArePerms' checks if 'smoothPerms' outputs correct permutations
-}
-- smoothPermsArePerms :: Int -> [Int] -> Bool
-- smoothPermsArePerms n xs = smoothPermIsPerm (perms xs) (smoothPerms n xs)

-- smoothPermIsPerm :: [[Int]] -> Bool
-- smoothPermIsPerm _ [] = True
-- smoothPermIsPerm xs (y : ys) = 

{-| 'smoothPermsAreUnique' checks if 'smoothPerms' outputs unique permutations 
-}
-- smoothPermsAreUnique :: Int -> [Int] -> Bool

{-| 'smoothPermsLength' checks if the length of output 
  is less than the length of the output of 'perms'
-}
-- smoothPermsLength :: Int -> [Int] -> Bool