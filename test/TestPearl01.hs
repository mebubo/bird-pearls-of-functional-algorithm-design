module TestPearl01 where

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Numeric.Natural (Natural)
import Data.List ((\\))
import Pearl01

compareImpls :: Minfree -> Minfree -> [Natural] -> Bool
compareImpls reference impl xs = impl xs == reference xs

compareWithNaive :: Minfree -> [Natural] -> Bool
compareWithNaive = compareImpls minfree

runTests :: Testable prop => [prop] -> IO Bool
runTests = ((all isSuccess) <$>) . sequence . (quickCheckResult <$>)

minOfTwo :: Minfree -> [Natural] -> [Natural] -> Bool
minOfTwo impl xs ys = min (impl xs) (impl ys) <= impl (xs ++ ys)

main :: IO Bool
main = do
   r1 <- runTests
        [ compareWithNaive minfree'
        , compareWithNaive minfree''
        ]
   r2 <- runTests
        [ minOfTwo minfree'
        , minOfTwo minfree''
        ]
   pure $ r1 && r2
