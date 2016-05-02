module Main where

import GCJ -- https://github.com/instinctive/gcjutils

import qualified Data.PQueue.Min as PQ

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Maybe (catMaybes)

main :: IO ()
main = single soln

soln :: IO ()
soln = do
    [aa,bb] <- words <$> getLine
    out $ solve aa bb
  where
    out (aa,bb) = putStrLn $ unwords [aa,bb]

data X = X
    { _abs  :: Int
    , _aa'  :: String
    , _bb'  :: String
    , _aa   :: String
    , _bb   :: String
    } deriving (Eq, Ord, Show)

solve :: String -> String -> (String, String)
solve aa0 bb0 = go . PQ.singleton $ X 0 [] [] aa0 bb0 where
    score = foldl' f 0 where f x c = x * 10 + digitToInt c
    raise = map f where f '?' = '9'; f c = c
    lower = map f where f '?' = '0'; f c = c
    mk aa' bb' aa bb = X (abs $ score aa' - score bb') aa' bb' aa bb
    chk q@(a,b) = if isDigit a && isDigit b then Just q else Nothing
    go pq =
        let (X _ aa' bb' aaa bbb, pq') = PQ.deleteFindMin pq in case (aaa,bbb) of
            ([],[])     -> (aa',bb')
            (a:aa,b:bb) -> go . PQ.union pq' . PQ.fromList . catMaybes $ xx where
                (eq,lt,gt) = case (a,b) of
                    ('?','?') -> (Just ('0','0'), Just ('0','1'), Just ('1','0'))
                    ('?', _ ) -> (Just (b,b), chk (pred b,b), chk (succ b,b))
                    ( _ ,'?') -> (Just (a,a), chk (a,succ a), chk (a,pred a))
                    ( _ , _ )
                        | a == b -> (Just (a,b), Nothing, Nothing)
                        | a  < b -> (Nothing, Just (a,b), Nothing)
                        | a  > b -> (Nothing, Nothing, Just (a,b))
                mkEQ (a,b) = mk (aa' ++ [a])             (bb' ++ [b])             aa bb
                mkLT (a,b) = mk (aa' ++ [a] ++ raise aa) (bb' ++ [b] ++ lower bb) [] []
                mkGT (a,b) = mk (aa' ++ [a] ++ lower aa) (bb' ++ [b] ++ raise bb) [] []
                xx = [ mkEQ <$> eq, mkLT <$> lt, mkGT <$> gt ]
