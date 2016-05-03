-- https://code.google.com/codejam/contest/8274486/dashboard#s=p2

module Main where

import GCJ -- https://github.com/instinctive/gcjutils

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    args <- getList
    out $ solve args
  where
    out = putStrLn . unwords . map show

modulus = 10^9 + 7 :: Integer

solve :: [Integer] -> [Integer]
solve [c,v,l] = [a `mod` modulus] where
    a = ww !! fromIntegral l
    ww = 1 : v : zipWith go (tail ww) ww
    go w1 w2 = v * w1 + c * v * w2
