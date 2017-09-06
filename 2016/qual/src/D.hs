module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import Data.List.Split (chunksOf)

main :: IO ()
main = single soln

soln :: IO ()
soln = getList >>= out . solve where
    out Nothing   = putStrLn "IMPOSSIBLE"
    out (Just xx) = putStrLn $ unwords (map show xx)

solve :: [Int] -> Maybe [Int]
solve [k,c,s]
    | s * c < k = Nothing
    | otherwise = Just $ ss
  where
    xx = map go $ chunksOf c [0..k-1]
    go = sum . zipWith (*) (map (k^) [0..]) . reverse
    ss = map (+1) xx
