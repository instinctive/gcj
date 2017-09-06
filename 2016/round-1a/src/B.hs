module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M

import Control.Monad (replicateM)
import Data.List (foldl', sort)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    n    <- getOne
    army <- replicateM (2 * n - 1) getList
    out $ solve army
  where
    out = putStrLn . unwords . map show

solve :: [[Int]] -> [Int]
solve army = sort . map fst . filter (odd.snd) $ M.toList m where
    m = foldl' go M.empty $ concat army where
        go m h = M.insertWith (+) h 1 m
