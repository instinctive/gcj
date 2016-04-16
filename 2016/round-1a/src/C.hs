module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M

import Data.List (find, foldl')
import Data.Maybe (catMaybes, fromJust)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    n    <- getOne
    bffs <- getList
    out $ solve n bffs
  where
    out = putStrLn . show

data D = Cycle Int | Pair Int Int Int deriving (Eq,Ord,Show)

solve :: Int -> [Int] -> Int -- M.Map (Int,Int) Int
solve n bffs = maximum (0 : (catMaybes $ map maxcycle cycles)) `max` pairlen where
    chains = foldl' go M.empty cycles
    pp = filter f $ M.keys chains where f (a,b) = a < b
    pairlen = sum $ map f pp where f (a,b) = 2 + chains M.! (a,b) + chains M.! (b,a)
    go m (Pair a b n) = M.insertWith max (a,b) n m
    go m _ = m
    maxcycle (Cycle n) = Just n
    maxcycle _ = Nothing
    m = M.fromList $ zip [1..] bffs
    grow xx@(x:_) = m M.! x : xx
    cycles = catMaybes $ map (classify . fromJust . find isCycle . iterate grow . (:[])) [1..n]
    isCycle (x:xx) = elem x xx
    classify (a:b:c:dd) | a == c   = Just $ Pair a b (length dd)
    classify (a:bb) | a == last bb = Just $ Cycle (length bb)
    classify _ = Nothing
