module SmoothPermsSlow.Internal where

{-| 'split' generates splits between any element of the list and the rest of the list
-}
split
  :: [a]        -- ^ input list '[a]'
  -> [(a, [a])] {-^ output list of pairs, 
    where the first element is the element that has been split from the list
    and the second element is the rest of the list
  -}
split []       = []
split (x : xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- split xs ]


{-| 'smooth' calculates if the permutation is smooth

  Given a list of integers xs and an integer d, 
  a smooth permutation of xs with maximum distance d is a permutation 
  in which the difference of any two consecutive elements is at less than d.
-}
smooth
  :: Int    -- ^ The maximum distance d
  -> [Int]  -- ^ the list of integers xs
  -> Bool   -- ^ True if the permutation is smooth
smooth n (x : y : ys) = abs (y - x) < n && smooth n (y : ys)
smooth _ _            = True
