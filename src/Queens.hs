import Ersatz
import Prelude hiding ((&&),(||),not,or)
import qualified Prelude
import Control.Monad (forM, when,unless)
import Control.Applicative
import System.Environment (getArgs)

main = do
    queens 8
   {- argv <- getArgs
    case argv of
        [s] -> queens (read s)-}

queens :: Int -> IO ()
queens n = do
  let positions = [(z,s)|z<-[0..n-1],s<-[0..n-1]]
  (res, bel) <- solveWith minisat $ do
    xs <- forM [1..n] $ \ z -> forM [1 .. n] $ \ s -> exists
    -- xs :: [[Bit]]
    forM [0..n-1] $ \ z -> assert $ or (xs !! z)
    let p (z,s) = xs !! z !! s
    -- p :: (Int,Int) -> Bit
    forM positions $ \ a -> forM positions $ \ b ->
      when (attacks a b) $ assert $ not (p a) || not (p b)
    {-forM positions $ \ a -> forM positions $ \ b ->
      unless (attacks a b) $ assert $ (p a) && (p b)-}
    return (xs :: [[Bit]])
  print (res, bel :: Maybe [[Bool]])
-- still unsat
attacks (z1,s1)(z2,s2) = (z1 == z2 ) Prelude.|| (s1 == s2) Prelude.|| (z1 - s1 == z2 - s2)

queen_pos = [(0,0),(1,6),(2,4),(3,7),(4,1),(5,3),(6,5),(7,2)]

-- equals to:     > [(x,y) | x <- xs, y <- ys]
-- also equals to:> liftM2 (,) xs xy
                  --cartProd :: [a] -> [b] -> [(a, b)]
--                > cartProd xs ys = (,) <$> xs <*> ys
selfCartProd :: [a] -> [(a,a)]
selfCartProd xs = (,) <$> xs <*> xs

test :: Bool -- True = all queen_pos will fail attacks == the wanted behaviour
test = Prelude.all (\ (x,y,b) -> Prelude.not b ) q_tuples

q_tuples = map (\ (x,y) -> (x,y,attacks x y )) $ filter (\ (x,y) -> x /= y) $ selfCartProd queen_pos
