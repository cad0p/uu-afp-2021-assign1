import           SmoothPermsTest

import           Test.QuickCheck                ( maxSize
                                                , quickCheck
                                                , quickCheckWith
                                                , stdArgs
                                                )

main :: IO ()
main = do
  quickCheck splitLength
  quickCheck splitLengthElems
  quickCheck splitElems

  -- https://devtut.github.io/haskell/quickcheck.html#limiting-the-size-of-test-data
  quickCheckWith (stdArgs { maxSize = 10 }) permsLength
  quickCheckWith (stdArgs { maxSize = 10 }) permsLengthElems
  quickCheckWith (stdArgs { maxSize = 10 }) permsElems