module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import Data.List ((\\))

main :: IO ()
main = single soln

soln :: IO ()
soln = getOne >>= out . solve where
    out Nothing  = putStrLn "INSOMNIA"
    out (Just x) = putStrLn $ show x

solve :: Int -> Maybe Int
solve 0 = Nothing
solve n = go 1 "0123456789" where
    go 1000 _ = error $ "solve: " ++ show n
    go i dd
        | null dd'  = Just $ i*n
        | otherwise = go (i+1) dd'
      where
        dd' = dd \\ show (i*n)
