-- pragmas {{{
{-# LANGUAGE TupleSections    #-}
-- }}}
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
import Data.Tuple
import System.IO (stderr, hPutChar, hFlush)
import qualified Debug.Trace as DBG
-- }}}

import Control.Lens
import Data.List ((\\))
import Linear.V2

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
type X = V2 Int
type Q = Char
type Model = (X,Q)

main :: IO ()
main = gcj $ do
    [n,m] <- getReadList
    mm <- replicateM m $ mk . words <$> getLine
    let (s, zz) = small n mm
    putStrLn $ unwords $ show <$> [s, length zz]
    for_ zz $ out
  where
    mk [s,r,c] = (V2 (read r) (read c), head s)
    out (V2 r c, q) = putStrLn . unwords $ [[q], show r, show c] 

small :: Z -> [Model] -> (Z, [Model])
small 1 [(_,'o')] = (2,[])
small 1 _ = (2,[(V2 1 1,'o')])
small n mm = (n*3-2, row1 <> diag <> rown \\ mm) where
    ocol = maybe 1 (view (_1._y)) $ find ((/='+').snd) mm
    row1 = [ (V2 1 c, q) | c <- [1..n], let q = bool '+' 'o' (c == ocol) ]
    diag = (,'x') <$> zipWith V2 [2..n] (cols \\ [ocol])
    cols = if ocol < n then [1..n] else [n,n-1..1]
    rown = (,'+') <$> V2 n <$> [2..n-1] 
