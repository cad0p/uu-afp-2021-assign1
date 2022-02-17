module SmoothPerms (perms, smoothPerms) where

import SmoothPerms.Internal (permTreeToPerms , listToPermTree)

smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms = replicate

{-| 'perms' generates permutations using 'PermTree'
-}
perms
  :: [Int]    -- ^ the input list
  -> [[Int]]  -- ^ the list of the permutations of input list
perms = permTreeToPerms . listToPermTree