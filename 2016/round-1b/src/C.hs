-- https://code.google.com/codejam/contest/11254486/dashboard#s=p2

module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Arrow (second)
import Control.Monad (replicateM)
import Data.List (foldl', transpose)
import Data.Maybe (maybeToList)

import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, stderr)

errStrLn :: String -> IO ()
errStrLn = hPutStrLn stderr

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    n      <- getOne
    topics <- replicateM n (words <$> getLine)
    -- errStrLn $ "n = " ++ show n
    print $ solve n topics

ids :: [String] -> (Int, [Int])
ids tt = (S.size s, map (m M.!) tt) where
    m = M.fromList $ zip (S.toList s) [1..]
    s = S.fromList tt

bfs :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
bfs goal next = go where
    go [] = []
    go (s:ss)
        | goal s    = s : go ss
        | otherwise = go (ss ++ next s)

dfs :: (a -> Bool) -> (a -> [a]) -> [a] -> [a]
dfs goal next = go where
    go [] = []
    go (s:ss)
        | goal s    = s : go ss
        | otherwise = go (next s ++ ss)

solve :: Int -> [[String]] -> Int
solve n topics = go us0 vs0 u2vs0 v2u0 where

    (us0,vs0,u2vs0,v2u0) = foldl' mk base $ zip uu vv where
        [tt1,tt2] = transpose topics
        (nu, uu) = ids tt1
        (nv, vv) = ids tt2
        base = (intset nu, intset nv, intmap nu, IM.empty)
        intset n = IS.fromDistinctAscList [1..n]
        intmap n = IM.fromAscList . zip [1..n] $ repeat IS.empty
        mk (us,vs,u2vs,v2u) (u,v)
            | IS.member u us && IS.member v vs =
                (IS.delete u us, IS.delete v vs, u2vs, IM.insert v u v2u)
            | otherwise = (us, vs, insert u v u2vs, v2u)

    insert a b m = IM.adjust (IS.insert b) a m
    delete a b m = IM.adjust (IS.delete b) a m

    go us vs u2vs v2u
        -- | unsafePerformIO (print $ IM.size v2u) `seq` False = 0
        -- | unsafePerformIO dbg `seq` False = 0
        | IS.null us || IS.null vs || null paths =
            n - IM.size v2u - IS.size us - IS.size vs
        | otherwise  = go us' vs' u2vs' v2u'
      where
        dbg = mapM_ errStrLn $
            [ ""
            , "us   " ++ show (IS.toList us)
            , "vs   " ++ show (IS.toList vs)
            , "u2vs " ++ show (ppmap u2vs)
            , "v2u  " ++ show (IM.toList v2u)
            ]
        ppmap = filter (not.null.snd) . map (second IS.toList) . IM.toList

        goal (_,(v:_),_) = IS.member v vs
        start =
            [ ([u],[v], xs)
            | u <- IS.toList us
            , let xs = u2vs IM.! u
            , v <- IS.toList xs
            ]
        next (uu,vv@(v:_),xs) =
            [ (u':uu,v':vv,IS.union xs xs')
            | u' <- maybeToList $ IM.lookup v v2u
            , not (elem u' uu)
            , let xs' = u2vs IM.! u'
            , v' <- IS.toList xs'
            , not (IS.member v' xs)
            ]

        paths = bfs goal next start
        ((uu,vv,_):_) = paths

        us' = IS.delete (last uu) us
        vs' = IS.delete (head vv) vs

        v2u' = foldl' ins v2u'' $ zip vv uu where
            ins m (v,u) = IM.insert v u m
            v2u'' = foldr IM.delete v2u (tail vv)

        u2vs' = foldl' ins u2vs'' $ zip uu (tail vv) where
            ins m (u,v) = insert u v m
            u2vs'' = foldl' del u2vs $ zip uu vv
            del m (u,v) = delete u v m
