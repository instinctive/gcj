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
main = gcj $ getReadList >>= putStrLn . unwords . map show . solve

solve :: [Z] -> [Z]
solve [n,k]
    | k-j <= m  = split (d+1)
    | otherwise = split  d
  where
    split g = [h+x,h] where (h,x) = divMod (g-1) 2
    (d,m) = divMod (n-j) (j+1)
    j = last . takeWhile (<k) $ pred.(2^) <$> [0..]
