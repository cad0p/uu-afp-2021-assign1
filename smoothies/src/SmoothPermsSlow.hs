module SmoothPermsSlow
  ( perms
  , smoothPerms
  ) where

-- https://stackoverflow.com/questions/14379185/function-privacy-and-unit-testing-haskell
import           SmoothPermsSlow.Internal       ( smooth
                                                , split
                                                )

{-| 'perms' generates permutations using 'split'
-}
perms
  :: [a]    -- ^ the input list '[a]'
  -> [[a]]  -- ^ the list of the permutations of '[a]'
perms [] = [[]]
perms xs = [ v : p | (v, vs) <- split xs, p <- perms vs ]


{-| 'smoothPerms' filters out 'perms' that are not 'smooth'
-}
smoothPerms
  :: Int      -- ^ the argument of 'smooth'
  -> [Int]    -- ^ the argument of 'perms'
  -> [[Int]]  -- ^ the smooth permutations
smoothPerms n xs = filter (smooth n) (perms xs)
