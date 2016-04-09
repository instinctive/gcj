module GCJ
    ( Handle
    , hGetLine
    , hGetInt, hGetInts
    , run, runFile, runHandle
    )
    where

import Control.Applicative ((<*>))
import Control.Monad (forM)
import System.IO (Handle, IOMode(..), stdin, hGetLine, withFile)

hGetInt :: Handle -> IO Int
hGetInt = fmap read . hGetLine

hGetInts :: Handle -> IO [Int]
hGetInts = fmap (map read . words) . hGetLine

hForEachCase :: Handle -> IO String -> IO [String]
hForEachCase h m =
    hGetInt h >>= \n ->
    forM [1..n] $ \i ->
    m         >>= \s ->
    return $ "Case #" ++ show i ++ ":" ++ s

runHandle :: (Handle -> IO String) -> Handle -> IO ()
runHandle f h = (hForEachCase <*> f) h >>= mapM_ putStrLn

runFile :: (Handle -> IO String) -> String -> IO ()
runFile f i = withFile i ReadMode $ runHandle f

run :: (Handle -> IO String) -> IO ()
run f = runHandle f stdin