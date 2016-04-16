module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M
import qualified Data.Array.Unboxed as A

import Data.Either (partitionEithers)
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

solve :: Int -> [Int] -> Int
solve n bffs = cmax `max` psum where

    bff i = a A.! i where
        a = A.listArray (1,n) bffs :: A.UArray Int Int

    -- cycles over length 2 must solely comprise the circle
    cmax = foldl' max 0 cycles

    -- all pairs can be in the circle, with their longest chains
    psum = M.size pmap + sum (M.elems pmap) where
        pmap = foldl' go M.empty pairs
        go m (ab,x) = M.insertWith max ab x m

    (pairs, cycles) = partitionEithers . catMaybes $ map go [1..n] where
        go = classify . fromJust . find isCycle . chain
        chain = iterate grow . (:[])
        grow xx@(x:_) = bff x : xx
        isCycle (x:xx) = elem x xx

        -- self-BFFs are not handled
        classify [a,b] | a == b        = error $ "self-BFF: " ++ show a
        classify (a:b:c:dd) | a == c   = Just . Left  $ ((a,b),length dd)
        classify (a:dd) | a == last dd = Just . Right $        length dd
        classify _                     = Nothing
