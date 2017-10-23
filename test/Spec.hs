module Main where

import Control.Monad (unless)
import qualified TestPearl01
import System.Exit (exitFailure)

main :: IO ()
main = do
    results <- sequence
        [ TestPearl01.main
        ]
    unless (all id results) exitFailure

