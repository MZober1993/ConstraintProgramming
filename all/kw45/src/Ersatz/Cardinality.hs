module Ersatz.Cardinality where

import Ersatz
import Prelude hiding ((&&),(||),not,or,and)
import qualified Prelude as P
import Data.List (foldl')

class Cardinality c where
  atleast :: Int -> c Bit -> Bit

  atmost  :: Int -> c Bit -> Bit
  atmost k xs = not $ atleast (k+1) xs

  exactly :: Int -> c Bit -> Bit
  exactly k  xs = atmost k xs && atleast k xs

newtype Rectangle b = Rectangle [b] 
instance Cardinality Rectangle where
  atleast k (Rectangle xs) = 
    if k <= 0  then true
    else if k > length xs - k
       then atmost (length xs - k) $ Rectangle $ map not xs
       else let ite b y n = -- choose n y b  <-- FIXME: why does this not work? 
                       (b && y) || (not b && n)
                insert bs x = zipWith (ite x) (true : bs) bs
            in  last $ foldl' insert (replicate k false) xs

newtype Direct b = Direct [b]
instance Cardinality Direct where
  atleast k (Direct xs) = 
    if k <= 0 then true
    else or $ map and $ sublists k xs

-- | all sublists of xs of length k,
-- example: sublists 3 [1,2,3,4] => [[2,3,4],[1,3,4],[1,2,4],[1,2,3]]
sublists :: Int -> [a] -> [[a]]
sublists k xs = 
  if k > 0 
  then case xs of 
    [] -> [] 
    x:ys -> sublists k ys ++ map (x:) (sublists (k-1) ys)
  else return []
