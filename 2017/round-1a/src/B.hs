module Main where
-- gcj imports {{{
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function (on)
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Ratio
import Data.Traversable
import Data.Tuple
import System.IO (stdout, stderr, hPutChar, hFlush)
import qualified Debug.Trace as Debug
-- }}}
-- my imports {{{
import Data.List (groupBy, sort, sortBy)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
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

getWords :: IO [String]
getWords = words <$> getLine

getRead :: Read a => IO a
getRead = read <$> getLine

getReadWords :: Read a => IO [a]
getReadWords = fmap read . words <$> getLine
-- }}}

type Z = Int
type V = (Z,Z)

main :: IO ()
main = gcj $ do
    [n,_] <- getReadWords
    amts  <- getReadWords
    pkgs  <- replicateM n getReadWords
    print $ solve n amts pkgs

lo :: Z -> Z -> Z
lo q x = quot (10 * x + d - 1) d where d = 11 * q

hi :: Z -> Z -> Z
hi q x = quot (10 * x) (9 * q)

data E = L | R deriving (Eq,Ord,Show)

events :: Z -> (Z,[Z]) -> [ ((Z,E),(Z,Z)) ]
events q (i,xx) = concat
    [ [ ((a,L),ip), ((b,R),ip) ]
    | (p,(a,b)) <- zip [0..] lrs
    , let ip = (i,p) ]
  where
    -- sort expiring intervals earlier
    lrs = sortBy (comparing snd)
        [ (a,b) | x <- xx
        , let a = lo q x
              b = hi q x
        , a <= b ]

type ZZMap = M.IntMap S.IntSet
type ZSet  = S.IntSet

quux :: (Z,[ZSet]) -> ZZMap -> (Z,[ZSet])
quux z@(n, uu) m
    | nk > 0 = (n + nk, zipWith use uu aa)
    | True   = z
  where
    -- packets that have not been used already
    aa = zipWith S.difference (M.elems m) uu where
    nk = minimum $ S.size <$> aa
    -- use the first nk available packets
    use u = foldr S.insert u . take nk . S.elems

solve :: Z -> [Z] -> [[Z]] -> Z
solve n qq xxx = answer where
    answer = fst $ foldl' quux (0, replicate n S.empty) ss
    -- maps of sets of aligned packets
    ss = scanl mk empty rl where
        empty = M.fromList $ zip [0..n-1] (repeat S.empty)
        mk m (rr,ll) = foldr ins (foldr del m rr) ll
        del (i,p) = M.adjust (S.delete p) i
        ins (i,p) = M.adjust (S.insert p) i
    -- events grouped (ending, starting)
    rl = mk $ [] : (map snd <$> groupBy eq ee) where
        eq ((_,a),_) ((_,b),_) = a == b
        mk (rr:ll:mmm) = (rr,ll) : mk mmm
        mk _ = []
    -- events
    ee = sort $ concat $ zipWith events qq (zip [0..] xxx)
