module Main where
-- gcj imports {{{
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Traversable
import System.IO (stderr, hPutChar, hFlush)
import qualified Debug.Trace as DBG
-- }}}
-- gcj {{{
gcj :: IO () -> IO ()
gcj docase = do
    t <- getRead
    for_ [1..t] $ \i -> do
        putStr $ "Case #" <> show i <> ": "
        docase
        hPutChar stderr '.'
        when (i `mod` 10 == 0) $ hPutChar stderr '\n'
        hFlush stderr

getRead :: Read a => IO a
getRead = read <$> getLine

getReadList :: Read a => IO [a]
getReadList = fmap read . words <$> getLine
-- }}}

type Z = Int

main :: IO ()
main = gcj $ getRead >>= putStrLn . solve

solve :: Z -> String
solve z = go z "" where
    go 0 s = s
    go z s
        | m == '0'  = go (d-1) (nines (m:s))
        | null s    = go d [m]
        | m <= c    = go d (m:s)
        | otherwise = go d (pred m : nines s)
      where
        (d,m) = second intToDigit $ divMod z 10
        c     = head s
        nines = fmap (const '9')
