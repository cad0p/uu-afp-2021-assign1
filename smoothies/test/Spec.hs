import           Test.Tasty
import           Test.Tasty.QuickCheck         as QC

import           SmoothPermsTest

main :: IO ()


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcSplitProps, qcPermsProps]


qcSplitProps :: TestTree
qcSplitProps = testGroup
  ("(checked by QuickCheck)\n" ++ "split")
  [ QC.testProperty "splitLength" splitLength
  , QC.testProperty "splitLengthElems" splitLengthElems
  , QC.testProperty "splitElems" splitElems
  ]

qcPermsProps :: TestTree
qcPermsProps = adjustOption
  (const (QuickCheckMaxSize 10))
  (testGroup
    ("(checked by QuickCheck)\n" ++ "perms")
    [ -- https://devtut.github.io/haskell/quickcheck.html#limiting-the-size-of-test-data
      QC.testProperty "permsLength" permsLength
    , QC.testProperty "permsLengthElems" permsLengthElems
    , QC.testProperty "permsElems" permsElems
  -- quickCheckWith (stdArgs { maxSize = 10 }) permsUnique
  -- does not work because they can be unique
    , QC.testProperty "smoothPermsAreSmooth" smoothPermsAreSmooth
    , QC.testProperty "smoothPermsArePerms" smoothPermsArePerms
    , QC.testProperty "smoothPermsLength" smoothPermsLength
    ]
  )
