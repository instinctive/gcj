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
-- my imports {{{
import Control.Lens
import Data.List ((\\))
import Linear.V2
import qualified Data.IntSet        as S
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

-- types {{{
type Z = Int
type X = V2 Int
type Q = Char
type Model = (X,Q)
-- }}}

main :: IO () -- {{{
main = gcj $ do
    [n,m] <- getReadList
    mm <- replicateM m $ mk . words <$> getLine
    let (s, zz) = small n mm
    putStrLn $ unwords $ show <$> [s, length zz]
    for_ zz $ out
  where
    mk [s,r,c] = (V2 (read r) (read c), head s)
    out (V2 r c, q) = putStrLn . unwords $ [[q], show r, show c] 
-- }}}

small :: Z -> [Model] -> (Z, [Model]) -- {{{
small 1 [(_,'o')] = (2,[])
small 1 _ = (2,[(V2 1 1,'o')])
small n mm = (n*3-2, row1 <> diags <> rown) where
    gaps = f <$> filter (not.(`S.member` cset)) [1..n] where f c = mk c '+'
    cset = S.fromList $ mm ^..traverse._1._y
    (ocol, row1) = case find ((/='+').snd) mm of
        Just (V2 1 c,'o') -> (c, gaps)
        Just (V2 1 c,'x') -> (c, mk c 'o' : gaps)
        Nothing -> case gaps of
            (V2 1 c,_) : rr -> (c, mk c 'o' : rr)
            otherwise -> (1, [mk 1 'o'])
    diags = (,'x') <$> zipWith V2 [2..n] (cc \\ [ocol]) where
        cc = if ocol < n then [1..n] else [n,n-1..1]
    rown = (,'+') <$> V2 n <$> [2..n-1] 
    mk c q = (V2 1 c, q)
-- }}}
