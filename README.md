# SolvableNonogram

The purpose of this project is to count the number of nonograms which are solvable with a particular solving system, which we will call **locally solvable** or solvable by **local logic**. This was inspired by [Pixelogic's "Every 5x5 Nonogram"](https://pixelogic.app/every-5x5-nonogram), which tries to get all 24976511 solvable 5x5 nonograms solved by a human. However, it is not clearly explained where this number of 5x5 nonograms came from. It is not simply the number of hints with a unique solution, as this number is documented by [OEIS sequence A242876](https://oeis.org/A242876), which says the number of 5x5 nonograms with a unique solution is 25309575, which differs from Pixelogic's number by 333064. Instead, this number comes from solving with local logic, explained in detail in the next paragraph. The number of locally solvable nonograms is not yet documented on the OEIS, but we know its first terms, for square grids of size 0 to 5, are 1, 2, 14, 384, 51234, 24976511. This project aims to find more terms for this and related sequences.

Local logic involves keeping track of deductions about each cell individually, and updating these deductions with new information deduced by only looking at single row or column. When tracking deductions about a cell, it can be in three states: definitely filled, definitely empty, or unknown. All cells start as unknown, and gradually become either definitely filled or definitely empty until solving ends. To update a cell, we look at either the row or column it is in and the hints for that. If all possibilities for that row or column which fit the clue and what is currently known about its contents force a particular cell to be always filled or empty, then we can update that cell as definitely filled or definitely empty. This is done iteratively until we reach one of three end points. First, if the grid has no unknown cells, then we have a locally solvable nonogram. Second, if it is impossible to make any deductions by looking at any rows or columns, then the nonogram is not locally solvable, which does not mean that it has no solution or multiple solutions, but merely that this system of logic is not powerful enough to deduce anything. Third, if looking at a particular row or column reveals that there are no possibilities that match the deductions and hints, then the nonogram is contradictory, and we know it definitely has no solution.

For 0x0 and 1x1 grids, any way of filling the grid will lead to unique hints, and will be locally solvable. For 2x2 and 3x3 grids, there are some hints that have multiple solutions, and thus cannot be solved by any system looking for a unique solution. All 2x2 and 3x3 nonograms with hints that lead to a unique solution are known to be locally solvable. It is only when we get to 4x4 grids that things become interesting, as there are hints that have a unique solution, but local logic is insufficient to find it. For example, the following hint has a unique solution, but local logic can make absolutely no progress in solving it:

```
   │1│1│1│ │
   │ │ │ │ │
   │1│1│1│1│
───┼─┼─┼─┼─┤
1 1│ │ │ │ │
───┼─┼─┼─┼─┤
1 1│ │ │ │ │
───┼─┼─┼─┼─┤
1 1│ │ │ │ │
───┼─┼─┼─┼─┤
  1│ │ │ │ │
───┴─┴─┴─┴─┘
```

This code is incredibly slow to run for large sizes, as it looks at every nxn grid, of which there are 2^(n^2), determines its hints, and attempts to apply inefficient logic to solve it. On a cheap laptop running `exe/numSolvable`, n=4 takes about 2 seconds, and n=5 takes about 42 minutes. My current estimate for n=6 is that it will take 31.9 years.

## Dependencies

Dependencies are kept minimal. Common bash utilities such as `make`, `sort`, `uniq`, and more are used. Also, `cabal` is used to compile haskell code. The cabal config file `SolvableNonogram.cabal` specifies versions of cabal and any libraries used.

## Building

Run `make` to build all of the executables and compute all of the data. If you only want to build the executables, run `make exe/all`. If you want to compute the data, run `make data/all`, though some steps in data generation do depend on the executables. If you only want to compute data files for certain numbers, you can run `make data/<n>/all` to make data for a size n, and if you want to compute only certain data for a size n, you can run `make data/<n>/<filename>` to only generate that data file, as well as any data files or executables needed to generate it.

Run `make clean` to delete all of the executables, data, and temporary files created in the build process, though the subdirectories of `data` will not be removed, so `make all` or `make data/all` will make their data again. To only delete the executables and temporary build files, you can run `make exe/clean`. To only delete data, you can run `make data/clean`, or to only remove data for a size n you can run `make data/<n>/clean`. Similar to `make clean`, this won't remove the actual subdirectories of `data`.

Every `clean` make target has a matching `fullclean` target that removes the directories as well.

## Running

Most of the executables are expected to be run only when making data. `exe/numSolvable` is different though, instead it is simply run on its own, given a size via standard input, and outputs the count by standard output. For example, the bash command `exe/numSolvable <<< "4"` will output `51234`.

## Data

Because the number of 5x5 nonograms is very large, it is hard to fit into memory, so intermediate data is stored on disk. This also allows prevents recalculation of earlier steps when later steps are being changed. The `data` directory stores all of this data.

The `data` directory only contains subdirectories representing different data sets. For example, `data/5` would contain data about 5x5 nonograms, while `data/4` would contain data about 4x4 nonograms. Currently, all of these directories must be named after a number.

The makefile has a target `data/all` which makes `data/<n>/all` for every directory in `data`. The target `data/<n>/all` makes all of the data files for size n. These depend on the executables, though they are set to not be recomputed if their executable changes, as the data should not become out of date even if the code that generated it changes. In practicular, if a library is refactored to make the code nicer, the generated data files should be the same regardless, so they are not regenerated.

A list of all data files follows:
* `data/<n>/size`: This file contains the side length of the nonogram square. It is generated from the directory name.
* `data/<n>/allGrids`: This file contains every possible grid.
* `data/<n>/allHints`: This file contains the corresponding hints for each grid in `data/allNonograms`. That is, line k of `data/allNonograms` would have hints on line k of `data/allHints`.
* `data/<n>/uniqueHints`: This file contains only those hints that have a unique grid, in no particular order.
* `data/<n>/numUniqueHints`: This file contains the number of hints that have a unique grid.
* `data/<n>/uniqueHintSols`: This file assigns a number to each hint in `data/<n>/uniqueHints` to represent its solvability. The number is 0 if the hint is contradictory (this should not happen since all hints originated from a filled grid), 1 is the hint can be solved using line by line logic, and 2 if it reaches a non-contradictory state where it can make no more deductions.
* `data/<n>/numUniqueSolvable`: This file counts how many of the hints in `data/<n>/uniqueHints` could be solved using line by line logic.
* `data/<n>/numUniqueUnsolvable`: This file counts how many of the hints in `data/<n>/uniqueHints` could not be solved using line by line logic because it got stuck in a position where no more deductions could be made.

## Executables

The data is generated with a mixture of bash and haskell code. Bash code is contained in the makefile, and haskell code is contained in the `app` and `lib` directories. Haskell code must be compiled before it is run, and the compiled executables are placed in the `exe` directory. Any helper files created during compilation are placed in the `dist-newstyle` directory. If the `exe` directory does not exist, it will be created.

The makefile has a target `exe/all`. which compiles all of the haskell executables.

A list of haskell executables follows:
* `exe/allGrids`: This is used to generate `data/<n>/allGrids`.
* `exe/makeHints`: This is used to generate `data/<n>/allHints`.
* `exe/solveHints`: This is used to generate `data/<n>/uniqueHintSols`.
* `exe/numSolvable`: This executable is different as it is not used to generate a data file. Instead, it takes as input a size from standard input, and outputs the number of locally solvable nonograms of that size to standard output. This is the equivalent of making `data/<n>/numUniqueSolvable`, but does everything in one step, skipping the part where duplicate hints are removed as it is too memory intensive and not necessary.

## Haskell Source Code

Haskell code for the executables is contained in the `app` directory, and general libraries that may be used by multiple executables are contained in the `lib` directory. Dependencies are managed by cabal, and can be seen in `SolvableNonogram.cabal`.

Haskell is used because it makes the code easier to read and write, though it is very inefficient when lots of data is stored in memory. Thus for tasks that involve looking at the list of all nonograms as a whole, bash is used instead, and haskell code is written to operate on each nonogram independently.

A list of haskell source files follows:
* `lib/Nonogram.hs`: This contains general definitions of nonogram grids and hints. This includes basic operations for conversion, extracting data, and writing and reading to files or the terminal, as well as generating hints from lines or grids of a puzzle.
* `app/allGrids.hs`: This is the source for `exe/allGrids`. Thanks to lazy evaluation, the list of all grids it generates never has to be entirely stored in memory, as the tail of the list is generated as needed, and the head of the list can be discarded once printed.
* `app/makeHints.hs`: This is the source for `exe/makeHints`. It contains a function to turn a grid into its hints, which is then applied to each line using `lines` and `unlines`, and then applied to standard input and output using `interact`. Using `lines`, `unlines`, and `interact` in this way means each line is computed independently, and so memory use is minimised.
* `lib/SolveClass.hs`: This contains general definitions useful for solving nonograms. This includes a class for deductions featuring methods for "either" and "both", classes for monads that support reading and updating the knowledge about the grid.
* `lib/SimpleGrid.hs`: This contains a simple and inefficient implementation of the monad in `lib/SolveClass.hs` for reading and updating the knowledge about the grid. This is mainly here as a demonstration of a simple implementation, and is not used in practice due to its inefficiency.
* `lib/ArrayGrid.hs`: This contains an array based implementation of the monad in `lib/SolveClass.hs` for reading and updating the knowledge about the grid.
* `lib/SolveLocally.hs`: This contains the actual logic of solving the grid. It is implemented only using the classes from `lib/SolveClass.hs`, so the underlying implementation can be chosen later.
* `app/solveHints.hs`: This is the source for `exe/solveHints`. Similar to `app/makeHints.hs`, a function is used to solve each hint, which is then efficiently applied to each line.
* `app/numSolvable.hs`: This is the source for `exe/numSolvable`. Similar to `app/allGrids.hs`, it generates a list of all grids lazily, and is able to apply a map-reduce to figure out which are locally solvable without excessive memory use, and remarkably little code.