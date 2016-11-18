import Ersatz
import Prelude hiding ((&&),(||),not,or)
import qualified Prelude as P
import Control.Monad (liftM2,forM, when)
import System.Environment (getArgs)

main = do
  argv <- getArgs
  case argv of
    [s] -> queens (read s)

queens :: Int -> IO ()
queens n = do
  let positions = [(z,s)|z<-[0..n-1],s<-[0..n-1]]
  (res, bel) <- solveWith minisat $ do
    xs <- forM [1..n] $ \ z -> forM [1 .. n] $ \ s -> exists
    forM [0..n-1] $ \ z -> assert $ or (xs !! z)
    let p (z,s) = xs !! z !! s
    forM positions $ \ a -> forM positions $ \ b ->
      when (attacks a b) $ assert $ not (p a) || not (p b)
    return (xs :: [[Bit]])
  print (res, bel :: Maybe [[Bool]])

attacks x y = rock x y  P.|| bischop x y
rock (z1,s1)(z2,s2) = ((z1 == z2 ) P.&& (s1 /= s2)) P.|| ((s1 == s2) P.&& (z1 /= z2))
bischop (z1,s1)(z2,s2) = ((z1 - s1 == z2 - s2) P.|| (z1 - z2 == s2 - s1)) P.&& (z1 /= z2)

test_all :: Bool -- return success
test_all = queen8_does_not_attack P.&& queen4_does_not_attack P.&& test_bischop P.&& test_rock P.&& P.not test_same_pos
-- positions that should not attack each other
queen8_pos = [(0,0),(1,6),(2,4),(3,7),(4,1),(5,3),(6,5),(7,2)]
queen4_pos = [(0,1),(1,3),(2,0),(3,2)]

queen8_does_not_attack = P.all (\ (x,y,b) -> P.not b ) $ q_tuples queen8_pos
queen4_does_not_attack = P.all (\ (x,y,b) -> P.not b ) $ q_tuples queen4_pos

q_tuples xs = map (\ (x,y) -> (x,y,attacks x y )) $ filter (\ (x,y) -> x /= y) $ self_cart_prod xs

self_cart_prod :: [a] -> [(a,a)]
self_cart_prod xs = liftM2 (,) xs xs

test_bischop=attacks (1,0)(2,1) && attacks (1,0)(2,1) && attacks (1,8)(2,7) && attacks (2,7)(1,8)
test_rock=attacks (1,1)(1,3) && attacks (2,3)(1,3)
test_same_pos=attacks (1,1)(1,1) && attacks (7,0)(7,0)
