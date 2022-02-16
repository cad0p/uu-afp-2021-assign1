import SmoothPermsTest

import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
  quickCheck splitLength
  quickCheck splitLengthElems
  quickCheck splitElems

