{-| Definitions for competing in the Google Code Jam.
-}

module GCJ (
    -- * Solve a codejam problem.
      single, multi
    -- * Useful input functions.
    , getOne, getList
    ) where

import Control.Monad (forM_)

-- | Read a single value from a stdin line.
getOne :: Read a => IO a
getOne = fmap read getLine

-- | Read a list of values from a stdin line.
getList :: Read a => IO [a]
getList = fmap (map read . words) getLine

-- | Read the number of test cases and run each case with single-line output.
single = run ": "

-- | Read the number of test cases and run each case with multi-line output.
multi  = run ":\n"

run sep m = getOne >>= mapM_ m' . enumFromTo 1 where
    m' i = (putStr $ "Case #" ++ show i ++ sep) >> m
