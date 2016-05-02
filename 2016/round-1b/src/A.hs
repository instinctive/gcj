module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.Map.Strict as M

import Data.List (foldl')

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    args <- getLine
    out $ solve args
  where
    out = putStrLn

digits :: [(Char,Char,String)]
digits =
    [ ('0', 'Z', "ZERO")
    , ('2', 'W', "TWO")
    , ('4', 'U', "FOUR")
    , ('6', 'X', "SIX")
    , ('8', 'G', "EIGHT")
    , ('3', 'R', "THREE")
    , ('7', 'S', "SEVEN")
    , ('5', 'V', "FIVE")
    , ('1', 'O', "ONE")
    , ('9', 'I', "NINE")
    ]

solve :: String -> String
solve s
    | sum (M.elems czero) /= 0 = error $ "czero: " ++ show czero
    | otherwise = concatMap (uncurry (flip replicate)) $ M.toList dmap
  where
    cmap = foldl' go M.empty s where go m c = M.insertWith (+) c 1 m
    (dmap,czero) = foldl' go (M.empty,cmap) digits where
        go (dm,cm) (d,c,ww) = (dm',cm') where
            (dm',cm') = case M.lookup c cm of
                Nothing -> (dm ,cm )
                Just n  -> (dm',cm') where
                    dm' = M.insert d n dm
                    cm' = foldl' f cm ww
                    f m w = M.adjust (+ (-n)) w m
