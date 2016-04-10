module Main where

import GCJ

import Data.List (foldl')
import qualified Data.Map.Strict as M

main :: IO ()
main = run soln MultiLine

soln :: Soln
soln = getList >>= out . solve where
    out = mapM_ (putLine . f)
    f (coin, dd) = coin ++ " " ++ unwords (map show dd)

solve :: [Int] -> [(String, [Integer])]
solve [n,j] = go M.empty jamcoins where
    jamcoins =
        [ (jam, map (`base` piece) [2..10])
        | piece <- cands (n `div` 2)
        , let jam = piece ++ piece
        ]
    go m jj
        | M.size m == j = M.toList m
        | null jj       = error $ "not enough"
        | otherwise     = go (insert m $ head jj) (tail jj)
    insert m (c,dd) = M.insert c dd m

cands :: Int -> [String]
cands n = map ('1' :) $ go (n-1) where
    go 1 = ["1"]
    go n = [ x:xx | x <- "01", xx <- go (n-1) ]

base :: Integer -> String -> Integer
base b = foldl' go 0 where
    go n '0' = n * b
    go n '1' = n * b + 1
    go _  c  = error $ "base.go: " ++ show c

