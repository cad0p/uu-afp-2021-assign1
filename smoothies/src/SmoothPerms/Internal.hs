module SmoothPerms.Internal where

data PermTree = Nil 
                  | Leaf  { toPerm    :: [Int]
                          , perm      :: [Int]      }
                  | Node  { toPerm    :: [Int]
                          , perm      :: [Int]
                          , children  :: [PermTree] }
                          deriving Show

newTree = do
  let n1 = Node { toPerm    = [1, 2]
                , perm      = []
                , children  = [] }
  print n1
  let n2 = Leaf { toPerm    = []
                , perm = [2, 1] }
  print n2