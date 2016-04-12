module Main where

import GCJ

import Data.List ((\\))

main :: IO ()
main = run soln Single

soln :: Soln
soln = getOne >>= out . solve where
    out Nothing  = putLine "INSOMNIA"
    out (Just x) = putLine $ show x

solve :: Int -> Maybe Int
solve 0 = Nothing
solve n = go 1 "0123456789" where
    go 1000 _ = error $ "solve: " ++ show n
    go i dd
        | null dd'  = Just $ i*n
        | otherwise = go (i+1) dd'
      where
        dd' = dd \\ show (i*n)
