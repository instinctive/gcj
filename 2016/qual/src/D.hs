module Main where

import GCJ

import Data.List.Split (chunksOf)

main :: IO ()
main = run soln Single

soln :: Soln
soln = getList >>= out . solve where
    out Nothing   = putLine "IMPOSSIBLE"
    out (Just xx) = putLine $ unwords (map show xx)

solve :: [Int] -> Maybe [Int]
solve [k,c,s]
    | s * c < k = Nothing
    | otherwise = Just $ ss
  where
    xx = map go $ chunksOf c [0..k-1]
    go = sum . zipWith (*) (map (k^) [0..]) . reverse
    ss = map (+1) xx
