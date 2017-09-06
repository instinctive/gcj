module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import Data.List (tails)

main :: IO ()
main = single soln

soln :: IO ()
soln = getLine >>= out . solve where
    out = putStrLn . show

solve :: String -> Int
solve ('-':xx) = 1 + solve (dropWhile (== '-') xx)
solve xx = (*2) . length . filter xo $ tails xx where
    xo ('+':'-':_) = True
    xo _           = False
