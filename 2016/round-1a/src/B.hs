module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M

import Control.Monad (replicateM)
import Data.List (delete, foldl', (\\), transpose, nub)
import Data.Maybe (catMaybes)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    n  <- getOne
    mm <- replicateM (2 * n - 1) getList
    out $ solve n mm
  where
    out = putStrLn . unwords . map show

army :: [[Int]]
army =
    [ [1,2,3]
    , [2,3,5]
    , [3,5,6]
    , [2,3,4]
    , [1,2,3]
    ]

build :: Int -> [[Int]] -> [[[Int]]]
build 0 _  = [[]]
build n vv = [ v : m | v <- vv, m <- build (n-1) (delete v vv), ok v m ]

ok v []    = True
ok v (z:_) = all id $ zipWith (<) v z

solve :: Int -> [[Int]] -> [Int]
solve n army = head $ catMaybes $ map aug cands where
    aug cand =
        let left = army \\ cand
            face = transpose cand
        in case face \\ left of
            [x] -> Just x
            _ -> Nothing
    cands = build n (nub army)
