module SmoothPermsTest where

import Test.QuickCheck (quickCheck)

-- import SmoothPermsSlow (perms)
import SmoothPermsSlow.Internal (split)

splitLength :: [a] -> Bool
splitLength xs = (length . split) xs == length xs

-- splitLengthElems :: [a] -> Bool 
-- splitLengthElems xs = all (map (\(a, [b]) -> a : b) (split xs)
splitLengthElems
  :: (Foldable t1, Foldable t2) => [(t1 a1, t2 a2)] -> Bool
splitLengthElems xs = 
  let 
    splitLengthElemsRec n [] = True 
    splitLengthElemsRec n (x : xs) = ((length (fst x) + length (snd x)) == n) && splitLengthElemsRec n xs
  in splitLengthElemsRec (length xs) xs