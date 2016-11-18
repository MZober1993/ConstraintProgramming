module Main where

import Prelude hiding ((||), (&&), not, and, or, all)
import OBDD.Data
import OBDD
import System.Process
import qualified Data.Set as S


xor a b = (a && not b) || (not a && b)
units n = map (`unit` True) [1 .. n]

xorFor n = foldl xor (unit 1 True) $ units n

g = let x1 = unit 1 True; x2 = unit 2 True; x3 = unit 3 True;  x4 = unit 4 True
    in not (x1 && x2) && not (x2 && x3) && not (x3 && x4)

k n = let xs = units n
    in and $ zipWith (\x y -> not (x && y)) xs $ tail xs


p n = foldl (\a b -> not (a && b)) (unit 1 True) $ units n

display d = readProcess "dot" ["-Tx11"] $ toDot d
count n = number_of_models (S.fromList [1 .. n])

main = do
    display $ xorFor 10
    display $ p 10
