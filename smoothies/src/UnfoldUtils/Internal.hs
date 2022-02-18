module UnfoldUtils.Internal where
-- https://github.com/tweag/rules_haskell/issues/152

import Prelude hiding ( iterate
                      , map )

-- Recall the definition of unfoldr for lists,

{-|
  >>> unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
  [10,9,8,7,6,5,4,3,2,1]
-}
unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next x = case next x of
                   Nothing     -> []
                   Just (y, r) -> y : unfoldr next r

-- We can define an unfold function for binary trees as well:

data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show

unfoldTree :: (s -> Either a (s, s)) -> s -> Tree a
unfoldTree next x = case next x of
                      Left  y      -> Leaf y
                      Right (l, r) -> Node (unfoldTree next l) (unfoldTree next r)


{-| The call iterate f x generates the infinite list [x, f x, f (f x), ...].
-}
iterate :: (a -> a) -> a -> [a]
iterate f = unfoldr (\x -> Just (x, f x))



map :: (a -> b) -> [a] -> [b]
map f = unfoldr next
  where
    next [] = Nothing
    next (x1 : xs) = Just(f x1, xs)

