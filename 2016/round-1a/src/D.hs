module Main where

import GCJ -- https://github.com/instinctive/gcjutils

main :: IO ()
main = single soln

soln :: IO ()
soln = getList >>= out . solve where
    out = putStrLn . show

solve :: [Int] -> Int
solve [x,y] = x + y
