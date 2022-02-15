module SmoothPermsSlow
    ( perms
    , smoothPerms
    ) where

-- https://stackoverflow.com/questions/14379185/function-privacy-and-unit-testing-haskell
import           SmoothPermsSlow.Internal       ( smooth
                                                , split
                                                )

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [ v : p | (v, vs) <- split xs, p <- perms vs ]

smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n xs = filter (smooth n) (perms xs)
