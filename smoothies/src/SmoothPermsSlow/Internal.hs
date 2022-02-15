module SmoothPermsSlow.Internal where

split :: [a] -> [(a, [a])]
split []     = []
split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]

smooth :: (Ord a, Num a) => a -> [a] -> Bool
smooth n (x:y:ys) = abs (y - x) < n && smooth n (y:ys)
smooth _ _        = True