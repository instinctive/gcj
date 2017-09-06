-- https://code.google.com/codejam/contest/8274486/dashboard

module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.IntSet as S

import Control.Arrow (first, second)
import Data.List (sort)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    n  <- getOne
    pp <- getList
    out $ solve n pp
  where
    out = putStrLn . unwords . map show

solve :: Int -> [Int] -> [Int]
solve n pp
    | sort (zz ++ ((`div` 3).(*4) <$> zz)) /= pp = error $ show (zz,pp)
    | otherwise = zz
  where
    zz = go n [] S.empty (prep 4) (prep 3)
    prep m = zip [0..] . map (*m) $ pp
    go 0 xx _ _ _ = (`div` 4) <$> reverse xx
    go m xx s aaa@((i,a):aa) bbb@((j,b):bb)
        | S.member i s = go m xx s aa bbb
        | S.member j s = go m xx s aaa bb
        | a == b = go (m-1) (a:xx) s' aa bb
        | a  < b = go m xx s aa bbb
        | a  > b = go m xx s aaa bb
      where
        s' = S.insert i $ S.insert j s
