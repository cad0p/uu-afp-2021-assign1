module SmoothPermsTest where

import Test.QuickCheck (quickCheck)

-- import SmoothPermsSlow (perms)
import SmoothPermsSlow.Internal (split)

{-| 'splitLength' checks if 'split' produces a list of pairs, of the same length
  of the original 'split' argument 'xs'
-}
splitLength :: [a] -> Bool
splitLength xs = (length . split) xs == length xs


{-| 'splitLengthElems' checks if every pair has a combined length of the same length
  of the original 'split' argument 'xs'
-}
splitLengthElems :: Foldable t0 => [t0 a0] -> Bool
splitLengthElems xs = splitLengthElem (length xs) (split xs)


splitLengthElem :: (Foldable t1, Foldable t2) => Int -> [(t1 a1, [t2 a2])] -> Bool
splitLengthElem _ [] = True 
splitLengthElem n ((_, []) : ys) = (1 == n) && splitLengthElem n ys
-- doesn't work, type issue, moving onto a custom generator
-- splitLengthElem n (([], y2) : ys) = ((1 + length y2) == n) && splitLengthElem n ys
splitLengthElem n ((y1, y2) : ys) = ((length y1 + length y2) == n) && splitLengthElem n ys


-- splitElems