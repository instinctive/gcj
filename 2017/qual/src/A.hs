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

main :: IO ()
main = gcj $ do
    [s,k] <- words <$> getLine
    putStrLn . maybe "IMPOSSIBLE" show $ solve s (read k)

solve :: String -> Int -> Maybe Int
solve s k = go 0 ((=='+') <$> s) where
    go n s 
        | null t       = Just n
        | length a < k = Nothing
        | otherwise    = go (n+1) $ map not a <> b
      where
        t = dropWhile id s
        (a,b) = splitAt k t
