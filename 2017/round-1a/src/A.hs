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
import qualified Debug.Trace as Debug
-- }}}
-- my imports {{{
import Control.Lens
import Data.Ix         ( range           )
import Data.List       ( partition, sort )
import Data.List.Split ( chunksOf        )
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

type Z   = Int
type Pt  = (Z,Z)
type Box = (Pt,Pt)

type Q   = Char
type Sq  = (Pt,Q)

vsplit :: Box -> Z -> (Box,Box)
vsplit box c = (lhs,rhs) where
    lhs = box & _2._2 .~ c-1
    rhs = box & _1._2 .~ c

hsplit :: Box -> Z -> (Box,Box)
hsplit box r = (top,bot) where
    top = box & _2._1 .~ r-1
    bot = box & _1._1 .~ r

fill :: Box -> Q -> [Sq]
fill box q = (,q) <$> range box

main :: IO ()
main = gcj $ do
    [r,c] <- getReadWords
    cake  <- replicateM r getLine
    putStrLn ""
    mapM_ putStrLn $ solve r c cake

solve :: Z -> Z -> [[Q]] -> [[Q]]
solve r c = toCake . go box . fromCake where
    box      = ((1,1),(r,c))
    toCake   = chunksOf c . fmap snd . sort
    fromCake = filter ((/='?').snd) . zip (range box) . concat

go :: Box -> [Sq] -> [Sq]
go b []      = error $ "go: " <> show b
go b [(_,q)] = fill b q
go b zz@(s:t:_)
    | rs == rt  = go lhs zlhs <> go rhs zrhs
    | otherwise = fill top q  <> go bot (tail zz)
  where
    ((rs,_),q)  = s
    ((rt,c),_)  = t
    (top,bot)   = hsplit b rt
    (lhs,rhs)   = vsplit b c
    (zlhs,zrhs) = partition ((<c).snd.fst) zz
