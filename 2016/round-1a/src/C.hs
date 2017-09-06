module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M
import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
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
    bff = listArray (1,n) bffs :: UArray Int Int
    cmax = foldl' max 0 cycles
    psum = sum $ M.elems pmap where
        pmap = foldl' insert M.empty pairs
        insert m (ab,x) = M.insertWith max ab (x+1) m

    (pairs, cycles) = partitionEithers . catMaybes $ map go [1..n] where
        go = classify . fromJust . find isCycle . chain
        chain = iterate grow . (:[])
        grow xx@(x:_) = bff ! x : xx
        isCycle (x:xx) = elem x xx

        classify [a,b] | a == b        = error $ "self-BFF: " ++ show a
        classify (a:b:c:dd) | a == c   = Just . Left  $ ((a,b),length dd)
        classify (a:dd) | a == last dd = Just . Right $        length dd
        classify _                     = Nothing
