module Pearl01 where

import Numeric.Natural (Natural)
import Data.List ((\\), partition)
import Data.Array (Array, Ix, elems, accumArray, assocs)
import Data.Array.ST (runSTArray, newArray, writeArray)

type Minfree = [Natural] -> Natural

minfree :: Minfree
minfree xs = head $ [0..] \\ xs

minfree' :: Minfree
minfree' = search . checklist

polyLength :: Num b => [a] -> b
polyLength = fromIntegral . length

search :: Num a => Array a Bool -> a
search = polyLength . takeWhile id . elems

checklist :: (Num a, Ord a, Ix a) => [a] -> Array a Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<=n) xs) (repeat True))
    where n = polyLength xs

countlist :: Int -> [Int] -> Array Int Int
countlist max xs = accumArray (+) 0 (0, max) (zip xs (repeat 1))

sort :: Int -> [Int] -> [Int]
sort max xs = concat [replicate k x | (x, k) <- assocs $ countlist max xs]

checklist' :: [Int] -> Array Int Bool
checklist' xs = runSTArray $ do
    a <- newArray (0, n) False
    sequence [writeArray a x True | x <- xs, x <= n]
    return a
    where n = length xs

minfree'' :: Minfree
minfree'' xs = minfrom 0 (polyLength xs, xs)

minfrom :: Natural -> (Natural, [Natural]) -> Natural
minfrom a (n, xs) | n == 0 = a
                  | m == b - a = minfrom b (n - m, vs)
                  | otherwise = minfrom a (m, us)
                    where (us, vs) = partition (< b) xs
                          b = a + 1 + n `div` 2
                          m = polyLength us
