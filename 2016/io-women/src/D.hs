-- https://code.google.com/codejam/contest/8274486/dashboard#s=p3

{-# LANGUAGE BangPatterns #-}

module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.List (foldl', intersect, partition, tails)
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    _  <- getLine
    pp <- words <$> getLine
    out $ solve ['A'..'Z'] $ filter nodups pp
  where
    out = putStrLn . fromMaybe "IMPOSSIBLE"
    nodups = not . any f . tails where
        f (x:xx) = elem x xx; f [] = False

solve :: String -> [String] -> Maybe String
solve alpha pp
    | null pp            = Just alpha
    | any (null.tail) pp = Nothing
    | isolated           = Nothing
    | otherwise          = answer
  where
    answer = listToMaybe $ go "" alphaset prev
    alphaset = S.fromList alpha
    asize = S.size alphaset
    (pp2,ppx) = partition (null.tail.tail) pp
    (next,prev) = foldl' f (M.empty,M.empty) pp2 where
        f (next,prev) [a,b] = (ins a b next, ins b a prev)
        ins a b m = M.insertWith S.union a (S.singleton b) m
    isolated =
        length next0 > 1 ||
        length prev0 > 1 || 
        not (null (intersect next0 prev0))
      where
        next0 = blocked next
        prev0 = blocked prev
        blocked = M.keys . M.filter ((==asize).S.size)
    go aa alphas prevs
        | S.null alphas = [aa]
        | null aa       = gen alphas
        | otherwise     = gen . S.difference alphas $ excluded
      where
        excluded = fromMaybe S.empty $ M.lookup (head aa) prevs
        gen cands = concat
            [ go aa' alphas' prevs'
            | a <- S.toList cands
            , let aa' = a:aa
            , not $ elem aa' ppx
            , let alphas' = S.delete a alphas
            , let prevs' = M.map (S.insert a) $ prevs
            , S.foldl' (blocked prevs') 0 alphas' <= 1
            ]
        blocked m n a = case M.lookup a m of
            Nothing -> n
            Just s  -> if S.size s == asize then n+1 else n
