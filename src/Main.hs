module Main where

num p q = 10 * p + q
n x = negate x
-- Kantenfärbung ohne K-3 rot oder blau
f = do
    p <- [1 .. 5]
    q <- [p+1 .. 5]
    r <- [q+1 .. 5]
    [[num p q, num q r,num p r, 0],[n $ num p q, n $ num q r,n $ num p r,0]]

--  n ( num p r 0)
main = putStrLn $ unlines $ do
    cl <- f
    return $ unwords $ map show cl

-- nächste Aufgabe: 2 Kantenfärbung des K-8 ohne rot K-3 ohne blau K-4
g = do
    p <- [1 .. 8]
    q <- [p+1 .. 8]
    r <- [q+1 .. 8]
    s <- [r+1 .. 8]
    [map (\l -> n l) [num p q, num q r,num r s,num p s, num q p, num r q,num s r,num s p, 0], [ num p q, num q r, num p r,0]]
    -- ohne blaue k-4 (konstruiere rot)                                                       ohne rote k-3 (konstruiere blau)