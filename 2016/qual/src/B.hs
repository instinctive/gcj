module Main where

import GCJ

import Data.List (tails)

import Data.List (splitAt)

main :: IO ()
main = run hCase

hCase :: Handle -> IO String
hCase h = hGetLine h >>= return . fmt . solve where
    fmt = (' ' :) . show

solve :: String -> Int
solve ('-':xx) = 1 + solve (dropWhile (== '-') xx)
solve xx = (*2) . length . filter xo $ tails xx where
    xo ('+':'-':_) = True
    xo _           = False
