module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import Data.List (sort)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    args <- getLine
    out $ solve args
  where
    out = putStrLn

solve :: String -> String
solve [] = []
solve (x:xx) = go xx ([x],[]) where
    go [] (aa,bb) = aa ++ reverse bb
    go (x:xx) (a:aa,bb)
        | x >= a    = go xx (x:a:aa,bb)
        | otherwise = go xx (a:aa,x:bb)
