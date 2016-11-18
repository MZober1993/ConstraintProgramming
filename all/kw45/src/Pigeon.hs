import Ersatz
import Prelude hiding ((&&),(||),not,or,and)
import qualified Prelude as P
import Control.Monad (replicateM, forM_)
import System.Environment (getArgs)
import Data.List (intersperse, transpose)

main = do
  [p,h] <- getArgs
  run (read p) (read h)

run pigeons holes = do
  (_ , cnf) <- runSAT $ build pigeons holes
  print cnf
  (Satisfied, Just xss) <- solveWith minisat $ build pigeons holes
  putStrLn $ unlines $ map ( intersperse ' ' . map (\c -> if c then '*' else '.')  ) xss

build pigeons holes = do
    xss <- replicateM pigeons $ replicateM holes exists
    forM_ xss $ \ row -> assert $ atleast 1 row
    forM_ (transpose xss) $ \ col -> assert $ atmost 1 col
    return (xss::[[Bit]])

atmost k xs = 
  if k < 0 then false 
  else and $ map (\ s -> or $ map not s ) $ sublists (k+1) xs

atleast k xs = not $ atmost (k-1) xs

-- | all sublists of xs of length k,
-- example: sublists 3 [1,2,3,4] => [[2,3,4],[1,3,4],[1,2,4],[1,2,3]]
sublists :: Int -> [a] -> [[a]]
sublists k xs = 
  if k > 0 
  then case xs of 
    [] -> [] 
    x:ys -> sublists k ys ++ map (x:) (sublists (k-1) ys)
  else return []
