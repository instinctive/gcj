-- https://code.google.com/codejam/contest/11254486/dashboard#s=p1

module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import Data.Char (isDigit)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    [aa,bb] <- words <$> getLine
    out $ solve aa bb
  where
    out (aa,bb) = putStrLn $ unwords [aa,bb]

solve :: String -> String -> (String, String)
solve aa0 bb0 = (aa,bb) where
    (_,aa,bb) = minimum . go "" $ zip aa0 bb0
    go s [] = [(0::Int,r,r)] where r = reverse s
    go s (z:zz) = case z of
        ('?','?') -> concat [ go s (    z':zz) | z' <- [('0','0'),('0','1'),('1','0')] ]
        ('?', q ) -> concat [ go s ((q',q):zz) | q' <- [q, pred q, succ q], isDigit q' ]
        ( q ,'?') -> concat [ go s ((q,q'):zz) | q' <- [q, pred q, succ q], isDigit q' ]
        ( a , b )
            | a == b -> go (a:s) zz
            | a  < b -> mk s ((a:).raise) ((b:).lower) zz
            | a  > b -> mk s ((a:).lower) ((b:).raise) zz
    raise = map f where f '?' = '9'; f c = c
    lower = map f where f '?' = '0'; f c = c
    mk s fa fb zz = [(abs $ read as - read bs, as, bs)] where
        r  = reverse s
        as = r ++ fa (fst <$> zz)
        bs = r ++ fb (snd <$> zz)
