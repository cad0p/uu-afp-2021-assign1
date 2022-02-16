module SmoothPerms.Internal where

import SmoothPermsSlow.Internal (split)

data PermTree = Nil 
              | Leaf  { perm      :: [Int]
                      , toPerm    :: [Int]      }
              | Node  { perm      :: [Int]
                      , toPerm    :: [Int]
                      , children  :: [PermTree] }
                      deriving Show

newTree = do
  let n1 = Node { perm    = [1, 2]
                , toPerm      = []
                , children  = [] }
  print n1
  let n2 = Leaf { perm    = []
                , toPerm = [2, 1] }
  print n2

{-| 
  https://www.dcode.fr/permutations-generator
  This is what the tree looks like
-}
listToPermTree :: [Int] -> PermTree
listToPermTree = listToNode []

listToNode 
  :: [Int]    -- ^ perm
  -> [Int]    -- ^ toPerm
  -> PermTree -- ^ Node or Leaf
listToNode xs [] = Leaf 
  { perm    = xs
  , toPerm  = [] }
listToNode xs ys = Node
  { perm      = xs
  , toPerm    = ys
  , children  = [
    listToNode xs' ys' | (xs'', ys') <- split ys
                       , xs' <- [xs'' : xs]
  ]}


-- permTreeToPerms :: PermTree -> [[Int]]
-- permTreeToPerms
