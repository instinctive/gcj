module Main where

import GCJ

import Data.List.Split (chunksOf)

main :: IO ()
main = run hCase

hCase :: Handle -> IO String
hCase h = hGetInts h >>= return . fmt . solve where
    fmt Nothing   = " IMPOSSIBLE"
    fmt (Just xx) = " " ++ unwords (map show xx)

solve :: [Int] -> Maybe [Int]
solve [k,c,s]
    | s * c < k = Nothing
    | otherwise = Just $ ss
--    | verify k c ss = Just $ ss
--    | otherwise     = error $ "solve: " ++ show [k,c,s]
  where
    xx = map go $ chunksOf c [0..k-1]
    go = sum . zipWith (*) (map (k^) [0..]) . reverse
    ss = map (+1) xx

orig 0 = [""]
orig k = [ x:xx | x <- "GL", xx <- orig (k-1) ]

grow o [] = []
grow o ('G':xx) = map (const 'G') o ++ grow o xx
grow o ( _ :xx) = o                 ++ grow o xx

cands k c = [ iterate (grow o) o !! (c-1) | o <- orig k ]

verify :: Int -> Int -> [Int] -> Bool
verify k c ss = all p (cands k c) where
    p aa = findGold aa ss || not (hasGold aa)

hasGold = any (=='G')
findGold aa = any (\s -> aa !! (s-1) =='G')
