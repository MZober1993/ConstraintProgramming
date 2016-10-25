module Queens where

import Prelude hiding ((&&),(||),not,or)
import Ersatz
import Control.Monad (forM,when)

main = do
    let n = 8 :: Int
        positions = [(z,s) | z <- [1 .. n-1], s<-[1 .. n-1]] :: [(Int,Int)]
    (res,bel) <- solveWith minisat $ do
        xs <- forM[1 .. n] $ \ z -> forM [1 .. n] $ \ s -> exists
        -- xs :: [[Bit]]
        forM [0 .. n - 1] $ \ z-> forM [0 .. n - 1] $ \ s-> assert $ or (xs !! z) && or (xs !! s)
        let p (z,s)= xs !! z !! s
        -- p :: (Int,Int) -> Bit
        forM positions $ \ a -> forM positions $ \ b ->
            when (attacks a b ) $ assert $ not (p a) || not (p b)
        return (xs :: [[Bit]])
    print (res,bel::Maybe [[Bool]])

attacks (z1,s1)(z2,s2) = z1 == z2

diagonal (z1,s1)(z2,s2)  = z1 == z2 +s2 - s1 && z2 ==z1+s1-s2
