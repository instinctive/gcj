GCJ
===

Definitions for competing in the Google Code Jam.

Example structure for a problem solution:

    module Main where
    import GCJ
    main = jam soln Single
    soln = getList >>= out . solve where
        out = putLine . show
    solve [x,y] = x + y

The `jam` function reads the number of test cases, prints the `Case #X:` statements, and correctly outputs the `Single` or `Multi` line output.

The `soln` function reads the input and writes the output for each test case.

The `solve` function is a purely functional solution to the problem.

Example: [Google Code Jam 2016 Qualification Round](https://github.com/instinctive/gcj2016q)
