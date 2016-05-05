-- https://code.google.com/codejam/contest/8274486/dashboard#s=p3

{-# LANGUAGE TupleSections #-}

module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.IntMap.Strict ((!))

import qualified Data.Map.Strict as M

import Data.List (foldl', delete, partition, tails)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Tuple (swap)

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
    | otherwise          = answer
  where

    a2i = (M.fromList (zip (alpha ++ "^$") [0..]) M.!) 
    i2a = (M.fromList (zip [0..] (alpha ++ "^$")) M.!)

    start = length alpha
    final = start + 1

    alphalst = [0..start-1]
    alphaset = IS.fromList alphalst

    mknext s f = IM.fromList $ (s, alphaset) : map mk alphalst where
        mk i = (i, IS.insert f $ IS.delete i alphaset)

    pwdfilter prev next pwds = (prev',next',pwds') where
        (p2,pwds') = partition (null.tail) pwds
        prev' = rem prev (swap.head <$> p2)
        next' = rem next (     head <$> p2)
        rem m = foldl' (flip f) m where f (a,b) = IM.adjust (IS.delete b) a

    answer = listToMaybe $ go start IM.empty prev next pwds where
        (prev,next,pwds) = pwdfilter prev0 next0 pwds0
        prev0 = mknext final start
        next0 = mknext start final
        pwds0 = mk . map a2i <$> pp where mk xx = zip xx (tail xx)

    go grow edges prev next pwds
        | IM.size edges == final = [word edges]
        | grow          == final = []
        | otherwise              = concat
            [ go grow' edges' prev' next' pwds'
            | (u,v) <- cands
            , let edges' = IM.insert u v edges
            , let grow' = if u == grow then follow v edges' else grow
            , let (prev',next',pwds') = pwdfilter (updnext v u prev) (updnext u v next) (updpwds u v pwds)
            ]
      where
        cands = blocked prev . blocked next . forced swap prev . forced id next $ ok
        ok = [ (grow, v) | v <- IS.toList $ next ! grow ]

    blocked m z = IM.foldr f z m where
        f s z
            | IS.size s == 0 = []
            | otherwise      = z
    forced g m z = IM.foldrWithKey f z m where
        f k s z
            | IS.size s == 1 = g.(k,) <$> IS.toList s
            | otherwise      = z

    updnext u v = IM.adjust (IS.delete u) v . IM.map (IS.delete v) . IM.delete u

    updpwds u v = filter (all f) . map (delete (u,v)) where f (a,b) = a /= u && b /= v

    follow a m = case IM.lookup a m of
        Nothing -> a
        Just b  -> if b == final then b else follow b m

    word m = go "" start where
        go s u = case IM.lookup u m of
            Nothing -> error $ "invalid edge: " ++ show (u,s,m)
            Just v
                | v == final -> reverse s
                | otherwise  -> go (i2a v : s) v
