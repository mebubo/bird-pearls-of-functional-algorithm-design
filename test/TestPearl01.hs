module TestPearl01 where

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Numeric.Natural (Natural)
import Data.List ((\\))
import Pearl01

compareImpls :: Minfree -> Minfree -> [Natural] -> Bool
compareImpls reference impl xs = impl xs == reference xs

compareWithNaive = compareImpls minfree

runTests :: Testable prop => [prop] -> IO Bool
runTests = ((all isSuccess) <$>) . sequence . (quickCheckResult <$>)

main :: IO Bool
main = do
   runTests
        [ compareWithNaive minfree'
        , compareWithNaive minfree''
        ]
