module SmoothPermsTest where

import Test.QuickCheck (quickCheck)

-- import SmoothPermsSlow (perms)
import SmoothPermsSlow.Internal (split)

splitLength :: [a] -> Bool
splitLength xs = (length . split) xs == length xs

splitLengthElems :: [a] -> Bool 
splitLengthElems xs = all (\tpl -> length (fst tpl) + length (snd tpl) == length xs) (split xs) 