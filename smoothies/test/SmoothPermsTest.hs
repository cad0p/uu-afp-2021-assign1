module SmoothPermsTest where

import Test.QuickCheck (quickCheck)

-- import SmoothPermsSlow (perms)
import SmoothPermsSlow.Internal (split)

{-| 'splitLength' checks if 'split' produces a list of pairs, of the same length
  of the original 'split' argument 'xs'
-}
splitLength :: [a] -> Bool
splitLength xs = (length . split) xs == length xs

{-| 'splitLengthElems' checks 
-}
splitLengthElems
  :: (Foldable t1, Foldable t2) => [(t1 a1, t2 a2)] -> Bool
splitLengthElems xs = 
  let 
    splitLengthElemsRec _ [] = True 
    splitLengthElemsRec n (y : ys) = ((length (fst y) + length (snd y)) == n) && splitLengthElemsRec n ys
  in splitLengthElemsRec (length xs) (split xs)

