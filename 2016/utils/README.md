GCJ
===

Definitions for competing in the Google Code Jam.

Example structure for a problem solution:

    module Main where
    import GCJ
    main = run soln Single
    soln = getList >>= out . solve where
        out = putLine . show
    solve [x,y] = x + y

The `run` and `runFile` functions will read the number of test cases, print the `Case #X:` statement, and correctly output the `Single` or `Multi` line output.

The `soln` function reads the input and writes the output for each test case.

The `solve` function is a purely functional solution to the problem.

Example: [Google Code Jam 2016 Qualification Round](https://github.com/instinctive/gcj2016q)
