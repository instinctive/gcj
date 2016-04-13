{-| Definitions for competing in the Google Code Jam.

    Example structure for a problem solution:

    @
    module Main where
    import GCJ
    main = run soln Single
    soln = getList >>= out . solve where
        out = putLine . show
    solve [x,y] = x + y
    @

    The 'run' and 'runFile' functions will read the number of test cases,
    print the "Case #X:" statement, and correctly output the 'Single' or
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
    -- * Run a problem.
    , run, runFile
    -- * Input and Output
    , getOne, getList, getString
    , putLine
    ) where

import Control.Monad (forM_)
import System.IO (Handle, IOMode(..), stdin, hGetLine, withFile)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow (second)

-- | The internal state: input file handle and output string producer.
type S = (Handle, String -> String)

-- | The internal state monad.
type Jam a = StateT S IO a

-- | The solution for a single test case.
type Soln = Jam ()

-- | The output style for this problem.
data Out
    = Single    -- ^ The output is on the same line as the "Case #X:" statement.
    | Multi     -- ^ The output is on multiple lines following the "Case #X:" statement.
    deriving Eq

mapLine :: (String -> a) -> Jam a
mapLine f = gets fst >>= liftIO . fmap f . hGetLine

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
putString s = modify $ second (.(s++))

-- | Output the string and a '\n'.
putLine :: String -> Jam ()
putLine s = modify $ second (.((s++).('\n':)))

hForEachCase :: Soln -> Out -> Handle -> IO String
hForEachCase m o h = fmap ($"") . flip evalStateT (h,id) $ do
    n <- getOne
    forM_ [1..n] $ \i -> putString (casestr i) >> m
    gets snd
  where
    casestr i = "Case #" ++ show i ++ final o
    final Single = ": "
    final Multi  = ":\n"

runHandle :: Soln -> Out -> Handle -> IO ()
runHandle m o h = hForEachCase m o h >>= putStr

-- | Run the solution on the specified file with output to stdout.
runFile :: Soln -> Out -> FilePath -> IO ()
runFile m o p = withFile p ReadMode $ runHandle m o

-- | Run the solution on stdin with output to stdout.
run :: Soln -> Out -> IO ()
run m o = runHandle m o stdin
