module GCJ
    ( Soln, Out(..)
    , getOne, getList, getString
    , putLine
    , run, runFile, runHandle
    )
    where

import Control.Monad (forM_)
import System.IO (Handle, IOMode(..), stdin, hGetLine, withFile)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow (second)

type S = (Handle, String)

type Jam a = StateT S IO a

type Soln = Jam ()

data Out = One | Multi deriving Eq

mapLine :: (String -> a) -> Jam a
mapLine f = gets fst >>= liftIO . fmap f . hGetLine

getOne :: Read a => Jam a
getOne = mapLine read

getList :: Read a => Jam [a]
getList = mapLine (map read . words)

getString :: Jam String
getString = mapLine id

putString :: String -> StateT S IO ()
putString s = modify $ second (++ s)

putLine :: String -> StateT S IO ()
putLine s = putString $ s ++ "\n"

hForEachCase :: Soln -> Out -> Handle -> IO String
hForEachCase m o h = flip evalStateT (h,"") $ do
    n <- getOne :: Jam Int
    forM_ [1..n] $ \i -> putString (casestr i) >> m
    gets snd
  where
    casestr i = "Case #" ++ show i ++ final o
    final One   = ": "
    final Multi = ":\n"

runHandle :: Soln -> Out -> Handle -> IO ()
runHandle m o h = hForEachCase m o h >>= putStr

runFile :: Soln -> Out -> FilePath -> IO ()
runFile m o p = withFile p ReadMode $ runHandle m o

run :: Soln -> Out -> IO ()
run m o = runHandle m o stdin
