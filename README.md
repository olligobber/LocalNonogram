# SolvableNonogram

The purpose of this project is to find nonograms with unique solutions that aren't solvable with certain solving logic. This was inspired by [Pixelogic's "Every 5x5 Nonogram"](https://pixelogic.app/every-5x5-nonogram), which tries to get all 24976511 solvable 5x5 nonograms solved by a human. However, looking at [OEIS sequence A242876](https://oeis.org/A242876), the number of 5x5 nonograms with a unique solution is 25309575. The difference of 333064 seems to be accounted for by which nonograms the Pixelogic team consider "solvable". This project attempts to find these "unsolvable" nonograms.

The current approach for what is considered "unsolvable" is based on trying to make progress by looking at each line individually. We keep track of whether each cell is either definitely filled, definitely empty, or unknown. Then, we look at each row and column and its hints, work out all the possible ways it could be filled that match our current knowledge and its hint, and update the grid with any new knowledge about cells that must always be filled or empty. This stops once we either reach a contradiction, or if none of the rows or columns individually can be used to make any progress.

## Dependencies

Dependencies are kept minimal. Common bash utilities such as `make`, `sort`, `uniq`, and more are used. Also, `ghc` is used to compile haskell code. GHC version 9.4.7 was used, though other versions will probably work fine.

## Building

Run `make` to build all of the executables and compute all of the data. If you only want to build the executables, run `make exe/all`. If you want to compute the data, run `make data/all`, though some steps in data generation do depend on the executables. If you only want to compute data files for certain numbers, you can run `make data/<n>/all` to make data for a size n, and if you want to compute only certain data for a size n, you can run `make data/<n>/<filename>` to only generate that data file, as well as any data files or executables needed to generate it.

Run `make clean` to delete all of the executables, data, and temporary files created in the build process, though the subdirectories of `data` will not be removed, so `make all` or `make data/all` will make their data again. To only delete the executables and temporary build files, you can run `make exe/clean`. To only delete data, you can run `make data/clean`, or to only remove data for a size n you can run `make data/<n>/clean`. Similar to `make clean`, this won't remove the actual subdirectories of `data`.

Every `clean` make target has a matching `fullclean` target that removes the directories as well.

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

The data is generated with a mixture of bash and haskell code. Bash code is contained in the makefile, and haskell code is contained in the `src` directory. Haskell code must be compiled before it is run, and the compiled executables are placed in the `exe` directory. Any helper files created during compilation are placed in the `build` directory, and may be reused if multiple haskell files rely on the same library. If the `exe` and `build` directories do not exist, they will be created.

The makefile has a target `exe/all`. which compiles all of the haskell executables.

A list of haskell executables follows:
* `exe/allGrids`: This is used to generate `data/<n>/allGrids`.
* `exe/makeHints`: This is used to generate `data/<n>/allHints`.
* `exe/solveHints`: This is used to generate `data/<n>/uniqueHintSols`.

## Haskell Source Code

Haskell code is contained in the `src` directory. So far this has no dependencies except what is included in GHC 9.4.7, and if that changes then either stack or cabal will be used to manage these.

Haskell is used because it makes the code easier to read and write, though it is very inefficient when lots of data is stored in memory. Thus for tasks that involve looking at the list of all nonograms as a whole, bash is used instead, and haskell code is written to operate on each nonogram independently.

A list of haskell source files follows:
* `src/Nonogram.hs`: This contains general definitions of nonogram grids and hints. This includes basic operations for conversion, extracting data, and writing and reading to files or the terminal, as well as generating hints from lines or grids of a puzzle.
* `src/allGrids.hs`: This is the source for `exe/allGrids`. Thanks to lazy evaluation, the list of all grids it generates never has to be entirely stored in memory, as the tail of the list is generated as needed, and the head of the list can be discarded once printed.
* `src/makeHints.hs`: This is the source for `exe/makeHints`. It contains a function to turn a grid into its hints, which is then applied to each line using `lines` and `unlines`, and then applied to standard input and output using `interact`. Using `lines`, `unlines`, and `interact` in this way means each line is computed independently, and so memory use is minimised.
* `src/SolveTools.hs`: This contains general definitions useful for solving nonograms. This includes a class for deductions featuring methods for "either" and "both", classes for monads that support reading and updating the knowledge about the grid, and a basic implementation of those classes.
* `src/solveHints.hs`: This is the source for `exe/solveHints`. Similar to `src/makeHints.sh`, a function is used to solve each hint, which is then efficiently applied to each line.