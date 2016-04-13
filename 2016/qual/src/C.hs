module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import Data.List (foldl')

main :: IO ()
main = run soln Multi

soln :: Soln
soln = getList >>= out . solve where
    out = mapM_ (putLine . f)
    f (coin, dd) = coin ++ " " ++ unwords (map show dd)

solve :: [Int] -> [(String, [Integer])]
solve [n,j]
    | odd n            = error $ "odd n: "            ++ show [n,j]
    | length coins < j = error $ "not enough coins: " ++ show [n,j]
    | otherwise        = coins
  where
    coins = take j
        [ (coin, map (`base` half) [2..10])
        | half <- cands (n `div` 2)
        , let coin = half ++ half
        ]

cands :: Int -> [String]
cands n = map ('1' :) $ go (n-1) where
    go 1 = ["1"]
    go n = [ x:xx | x <- "01", xx <- go (n-1) ]

base :: Integer -> String -> Integer
base b = foldl' go 0 where
    go n '0' = n * b
    go n '1' = n * b + 1
    go _  c  = error $ "invalid digit: " ++ show c
