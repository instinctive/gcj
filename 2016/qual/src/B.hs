module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import Data.List (tails)

main :: IO ()
main = jam soln Single

soln :: Soln
soln = getString >>= out . solve where
    out = putLine . show

solve :: String -> Int
solve ('-':xx) = 1 + solve (dropWhile (== '-') xx)
solve xx = (*2) . length . filter xo $ tails xx where
    xo ('+':'-':_) = True
    xo _           = False
