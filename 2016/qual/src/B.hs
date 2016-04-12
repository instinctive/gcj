module Main where

import GCJ

import Data.List (tails)

main :: IO ()
main = run soln One

soln :: Soln
soln = getString >>= out . solve where
    out = putLine . show

solve :: String -> Int
solve ('-':xx) = 1 + solve (dropWhile (== '-') xx)
solve xx = (*2) . length . filter xo $ tails xx where
    xo ('+':'-':_) = True
    xo _           = False
