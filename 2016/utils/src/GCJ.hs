{-| Definitions for competing in the Google Code Jam.

    Example structure for a problem solution:

    @
    module Main where
    import GCJ
    main = jam soln Single
    soln = getList >>= out . solve where
        out = putLine . show
    solve [x,y] = x + y
    @

    The 'jam' and 'jamFile' functions will read the number of test cases,
    print the @Case #X:@ text, and correctly output the 'Single' or
    'Multi' line output.

    The 'soln' function reads the input and writes the output for each
    test case.

    The 'solve' function is a purely functional solution to the problem.
-}

module GCJ (
    -- * Types
      Soln, Out(..)
    -- ** Internal Types
    , Jam, S
    -- * Solve a codejam problem.
    , jam, jamCase, jamFile
    -- * Input and Output
    , getOne, getList, getString
    , putLine
    ) where

import Control.Monad (forM_)
import System.IO (Handle, IOMode(..), stdin, hGetLine, withFile)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow (second)

-- | The internal state: input file handle and output string producer.
type S = Handle

-- | The internal state monad.
type Jam a = StateT S IO a

-- | The solution for a single test case.
type Soln = Jam ()

-- | The output style for this problem.
data Out
    = Single    -- ^ The output is on the same line as the @Case #X:@ statement.
    | Multi     -- ^ The output is on multiple lines following the @Case #X:@ statement.
    deriving Eq

mapLine :: (String -> a) -> Jam a
mapLine f = get >>= liftIO . fmap f . hGetLine

-- | Read a single value from an input line.
getOne :: Read a => Jam a
getOne = mapLine read

-- | Read a list of values from an input line.
getList :: Read a => Jam [a]
getList = mapLine (map read . words)

-- | Return the input line as a string (without a final '\n').
getString :: Jam String
getString = mapLine id

putString :: String -> Jam ()
putString = liftIO . putStr

-- | Output the string and a '\n'.
putLine :: String -> Jam ()
putLine = liftIO . putStrLn

jamHandle :: Soln -> Out -> Handle -> IO ()
jamHandle m o h = flip evalStateT h $ do
    n <- getOne
    forM_ [1..n] $ \i -> putString (casestr i) >> m
  where
    casestr i = "Case #" ++ show i ++ final o
    final Single = ": "
    final Multi  = ":\n"

-- | Read a problem from a file, solution to stdout.
jamFile :: Soln -> Out -> FilePath -> IO ()
jamFile m o p = withFile p ReadMode $ jamHandle m o

-- | Read a problem from stdin, solution to stdout.
jam :: Soln -> Out -> IO ()
jam m o = jamHandle m o stdin

-- | Read a case from stdin, solution to stdout.
jamCase :: Soln -> Out -> IO ()
jamCase m o = flip evalStateT stdin m
