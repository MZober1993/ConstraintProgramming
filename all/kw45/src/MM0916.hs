import Ersatz
import Ersatz.Cardinality
import Prelude hiding ((&&),(||),not,or,and)
import qualified Prelude as P
import Control.Monad (replicateM, forM_, guard, void, when)
import Data.List (intersperse, transpose, foldl')
import System.Environment (getArgs)
import System.Directory

main = do
  args <- getArgs
  let card = Rectangle
  case args of
    [n,q,a,s] -> void $ run card (read n) (read q) (read a) (read s)
    [n,q,a]   -> search card (read n) (read q) (read a) 
    [n,q] -> forM_ [0..8] $ \ a -> search card (read n)(read q) a

search card n q a = do
  let go s = do
        ok <- run card n q a s
        when ok $ go (s+1)
  go 1

test1 = run Direct 4 2 0 3
test2 = run Direct 5 4 0 4

type Position = (Int,Int)

positions :: Int -> [Position]
positions n = do x <-[0..n-1] ; y <- [0..n-1] ; return (x,y)

type Direction = (Int,Int)

directions :: [Direction]
directions = [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]

scale :: Int -> Direction -> Direction
scale s (dx,dy) = (s*dx,s*dy)

shift :: Direction -> Position -> Position
shift (dx,dy) (x,y) = (x+dx,y+dy)

onboard :: Int -> Position -> Bool
onboard n (x,y) = P.and [ 0 <= x, x < n, 0 <= y, y < n ]

run :: Cardinality c => ([Bit] -> c Bit) -> Int -> Int -> Int -> Int -> IO Bool
run card n q a s = do
  (status,res) <- solveWith minisat $ do
    qss <- replicateM n $ replicateM n exists
    ass <- replicateM n $ replicateM n exists
    let queen (x, y) = qss !! x !! y
    	attacked (x, y) = ass !! x !! y
    assert $ exactly q $ card $ concat qss
    let blanks = n * n - (q + s)
    if blanks <= s 
      then assert $ atmost blanks
                  $ card
                  $ map not
                  $ zipWith (||) (concat qss) (concat ass)
      else assert $ atleast s $ card $ concat ass
    forM_ (positions n) $ \ p ->
      assert $ not $ queen p && attacked p
    forM_ (positions n) $ \ p -> 
      assert $ attacked p ==> exactly a ( card $ do
            d <- directions
	    return $ or $ do
	      s <- [1 .. n]
	      let q :: Position
	          q = shift (scale s d) p
              guard $ onboard n q
	      return $ queen q )
    return (qss::[[Bit]],ass::[[Bit]])  
  case (status,res) of
     (Satisfied,Just(qss,ass)) -> do
        let header = unwords [ "N", "=", show n, "Q", "=", show q, "A", "=", show a, "S", "=", show s ]
        let [c] = show a
            letter (q,a) = if q then 'Q' else if a then '+' else '.'
            info = unlines $ header : map ( intersperse ' ' . map letter ) (zipWith zip qss ass)
        putStrLn info
        writeFile (fname n q a s) info
        let prev = fname n q a $ s-1
        ex <- doesFileExist prev
        when ex $ removeFile prev
        return True
     _-> return False

fname n q a s = concat ( intersperse "-" [show n, show q, show a, show s] ) ++ ".txt"

