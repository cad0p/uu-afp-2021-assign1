module SmoothPerms.Internal where

data PermTree a = Nil 
                  | Leaf  { toPerm    :: a
                          , perm      :: a }
                  | Node  { toPerm    :: a
                          , perm      :: a
                          , children  :: [PermTree a]}
                          deriving Show

newTree = do
  let n1 = Node { toPerm    = [1, 2]
                , perm      = []
                , children  = []}
  print n1