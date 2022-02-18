module SmoothPerms (perms, smoothPerms) where

import SmoothPerms.Internal (  permTreeToPerms
                             , listToPermTree
                             , listToSmoothPermTree )

{-| 'smoothPerms' generates smooth permutations
  only expanding the 'PermTree' if the possible permutations
  can be smooth (it does not expand all the leaves)
-}
smoothPerms 
  :: Int      -- ^ the argument of 'smooth'
  -> [Int]    -- ^ the argument of 'perms'
  -> [[Int]]  -- ^ the list of the smooth permutations
smoothPerms n xs = permTreeToPerms(listToSmoothPermTree n xs)

{-| 'perms' generates permutations using 'PermTree'
-}
perms
  :: [Int]    -- ^ the input list
  -> [[Int]]  -- ^ the list of the permutations of input list
perms = permTreeToPerms . listToPermTree