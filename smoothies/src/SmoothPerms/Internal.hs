module SmoothPerms.Internal where

import SmoothPermsSlow.Internal (split, smooth)

data PermTree = Nil 
              | Leaf  { perm      :: [Int]
                      , toPerm    :: [Int]      }
              | Node  { perm      :: [Int]
                      , toPerm    :: [Int]
                      , children  :: [PermTree] }
                      deriving Show

-- newTree = do
--   let n1 = Node { perm    = [1, 2]
--                 , toPerm      = []
--                 , children  = [] }
--   print n1
--   let n2 = Leaf { perm    = []
--                 , toPerm = [2, 1] }
--   print n2

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


{-|
  'permTreeToPerms' should go depth first and when it finds
  a leaf it should return it

  Example: (permTreeToPerms . listToPermTree) [1,2]
  Returns: [[2,1],[1,2]]
  https://stackoverflow.com/questions/49989439/check-type-of-parameter-in-haskell
-}
permTreeToPerms :: PermTree -> [[Int]]
permTreeToPerms Nil = [[]]
permTreeToPerms (Node _ _ c) = concat [permTreeToPerms x | x <- c]
permTreeToPerms (Leaf p _) = [p]


{-| 'pruneSmooth' leaves only smooth permutations in the tree

  I think we should directly build the tree by not expanding
  instead of pruning, so I will go this direction.
-}


listToSmoothPermTree :: Int -> [Int] -> PermTree
listToSmoothPermTree = listToSmoothNode []

listToSmoothNode 
  :: [Int]    -- ^ perm
  -> Int      -- ^ 'smooth' parameter n
  -> [Int]    -- ^ toPerm
  -> PermTree -- ^ Node or Leaf
listToSmoothNode xs _ [] = Leaf 
  { perm    = xs
  , toPerm  = [] }
listToSmoothNode [] n ys = Node -- the first split is assumed smooth
  { perm      = []
  , toPerm    = ys
  , children  = [
    listToSmoothNode xs' n ys' | (xs'', ys') <- split ys
                       , xs' <- [xs'' : []]
  ]}
listToSmoothNode (x : xs) n ys = Node
  { perm      = x : xs
  , toPerm    = ys
  , children  = [
    listToNode xs' ys' | (xs'', ys') <- smoothSplit n x ys
                       , xs' <- [xs'' : x : xs]
  ]}


{-| 'smoothSplit' only returns the splits that preserve
  the smoothess, thus pruning 'PermTree' to make it smooth

  https://stackoverflow.com/questions/1618462/filtering-list-of-tuples
-}
smoothSplit
  :: Int -- ^ 'smooth' parameter n
  -> Int -- ^ the first element of the 'perm' list
  -> [Int] -- ^ the argument of 'split'
  -> [(Int, [Int])] -- ^ the return type of 'split'
smoothSplit n x ys = filter (\(s, _) -> smooth n (x : [s])) (split ys)




