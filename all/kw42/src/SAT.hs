module SAT where

import Prelude hiding ((&&))
import Ersatz

main = do
    solveWith minisat (do p <- exists;q <- exists; assert (p && q); return [p,q]) :: IO (Result, Maybe [Bool])