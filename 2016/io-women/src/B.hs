-- https://code.google.com/codejam/contest/8274486/dashboard#s=p1

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

solve :: [Int] -> [Int]
solve [d,k,n] = [y,z] where
    kpos = pos k
    ypos = kpos + 1
    zpos = kpos - 1
    (y,z)
        | odd k     = (edance ypos, edance zpos)
        | otherwise = (odance ypos, odance zpos)
    pos i
        | odd i     = (i + n) `mod` d
        | otherwise = (i - n) `mod` d
    edance p = corr $ (p + n) `mod` d
    odance p = corr $ (p - n) `mod` d
    corr 0 = d
    corr i = i
