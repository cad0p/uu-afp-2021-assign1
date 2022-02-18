import           Test.Tasty
import           Test.Tasty.QuickCheck         as QC

import           SmoothPermsTest
import qualified SmoothPerms as Fast
import qualified SmoothPermsSlow as Slow

main :: IO ()


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "QuickCheck"  [ qcSmoothPermsSlow
                                  , qcSmoothPerms]

qcSmoothPermsSlow :: TestTree
qcSmoothPermsSlow = testGroup
  "SmoothPermsSlow"
  [ qcSmoothPermsSlowSplitProps
  , qcSmoothPermsSlowPermsProps ]


qcSmoothPermsSlowSplitProps :: TestTree
qcSmoothPermsSlowSplitProps = testGroup
  "split"
  [ QC.testProperty "splitLength" splitLength
  , QC.testProperty "splitLengthElems" splitLengthElems
  , QC.testProperty "splitElems" splitElems
  ]

{-| Regarding QuickCheckMaxSize
  https://cardanoupdates.com/commits/3df1289383266266382bf5a00fe5d8654d913218
  which substitutes `quickCheckWith (stdArgs { maxSize = 10 }) permsLength`
-}
qcSmoothPermsSlowPermsProps :: TestTree
qcSmoothPermsSlowPermsProps = adjustOption
  (const (QuickCheckMaxSize 10))
  (testGroup
    "perms"
  --   [ -- https://devtut.github.io/haskell/quickcheck.html#limiting-the-size-of-test-data
  --     QC.testProperty "permsLength" permsLength
  --   , QC.testProperty "permsLengthElems" permsLengthElems
  --   , QC.testProperty "permsElems" permsElems
  -- -- quickCheckWith (stdArgs { maxSize = 10 }) permsUnique
  -- -- does not work because they can be unique
  --   , QC.testProperty "smoothPermsAreSmooth" (smoothPermsAreSmooth
  --       Slow.smoothPerms)
  --   , QC.testProperty "smoothPermsArePerms" (smoothPermsArePerms
  --       Slow.smoothPerms)
  --   , QC.testProperty "smoothPermsLength" (smoothPermsLength
  --       Slow.smoothPerms)
  --   ]
    (let  x = Slow.perms
          y = Slow.smoothPerms
     in 
    [
      QC.testProperty "permsLength" (permsLength x)
    , QC.testProperty "permsLengthElems" (permsLengthElems x)
    , QC.testProperty "permsElems" (permsElems x)
    , QC.testProperty "smoothPermsAreSmooth" (smoothPermsAreSmooth y)
    , QC.testProperty "smoothPermsArePerms" (smoothPermsArePerms y)
    , QC.testProperty "smoothPermsLength" (smoothPermsLength y)
    ])
  )


qcSmoothPerms :: TestTree
qcSmoothPerms = testGroup 
  "SmoothPerms"
  [ qcSmoothPermsProps ]


qcSmoothPermsProps :: TestTree
qcSmoothPermsProps = adjustOption
  (const (QuickCheckMaxSize 10))
  (testGroup
    "perms"
    (let  x = Fast.perms
          y = Fast.smoothPerms
     in 
    [
      QC.testProperty "permsLength" (permsLength x)
    , QC.testProperty "permsLengthElems" (permsLengthElems x)
    , QC.testProperty "permsElems" (permsElems x)
    , QC.testProperty "smoothPermsAreSmooth" (smoothPermsAreSmooth y)
    , QC.testProperty "smoothPermsArePerms" (smoothPermsArePerms y)
    , QC.testProperty "smoothPermsLength" (smoothPermsLength y)
    ])
  )