module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M
import qualified Data.Array.Unboxed as A

import Data.List (find, foldl')
import Data.Maybe (fromJust)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    n    <- getOne
    bffs <- getList
    out $ solve n bffs
  where
    out = putStrLn . show

type PMap = M.Map (Int,Int) Int

solve :: Int -> [Int] -> Int
solve n bffs = cmax `max` pmax where

    bff i = a A.! i where a = A.listArray (1,n) bffs :: A.UArray Int Int

    pmax = foldl' go 0 canon where
        go x (a,b) = x + 2 + pmap M.! (a,b) + pmap M.! (b,a)
        canon = filter (uncurry (<)) (M.keys pmap)

    (pmap, cmax) = foldl' go (M.empty, 0) chains where
        chains = map mkChain [1..n] where
            mkChain = fromJust . find isCycle . iterate grow . (:[])
            grow xx@(x:_) = bff x : xx
            isCycle (x:xx) = elem x xx

        go (pmap, cmax) (a:b:c:dd) | a == c = (pmap', cmax) where
            pmap' = M.insertWith max (a,b) (length dd) pmap

        go (pmap, cmax) (a:dd) | a == last dd = (pmap, cmax') where
            cmax' = cmax `max` length dd

        go q _ = q
