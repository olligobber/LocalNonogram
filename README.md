# LocalNonogram

The purpose of this project is to count the number of nonograms which are solvable with a particular solving system, which we will call **locally solvable** or solvable by **local logic**. This was inspired by [Pixelogic's "Every 5x5 Nonogram"](https://pixelogic.app/every-5x5-nonogram), which tries to get all 24976511 solvable 5x5 nonograms solved by a human. However, it is not clearly explained where this number of 5x5 nonograms came from. It is not simply the number of hints with a unique solution, as this number is documented by [OEIS sequence A242876](https://oeis.org/A242876), which says the number of 5x5 nonograms with a unique solution is 25309575, which differs from Pixelogic's number by 333064. Instead, this number comes from solving with local logic, explained in detail in the next paragraph. Since I started working on this project, the number of locally solvable nonograms for square grids has been added to the OEIS as [sequence A391619](https://oeis.org/A391619). However, the number of rectangular grids has not yet been added, though it exists for uniquely solvable nonograms as [sequence A384764](https://oeis.org/A384764).

Local logic involves keeping track of deductions about each cell individually, and updating these deductions with new information deduced by only looking at single row or column. When tracking deductions about a cell, it can be in three states: definitely filled, definitely empty, or unknown. All cells start as unknown, and gradually become either definitely filled or definitely empty until solving ends. To update a cell, we look at either the row or column it is in and the hints for that. If all possibilities for that row or column which fit the clue and what is currently known about its contents force a particular cell to be always filled or empty, then we can update that cell as definitely filled or definitely empty. This is done iteratively until we reach one of three end points. First, if the grid has no unknown cells, then we have a locally solvable nonogram. Second, if it is impossible to make any deductions by looking at any rows or columns, then the nonogram is not locally solvable, which does not mean that it has no solution or multiple solutions, but merely that this system of logic is not powerful enough to deduce anything. Third, if looking at a particular row or column reveals that there are no possibilities that match the deductions and hints, then the nonogram is contradictory, and we know it definitely has no solution.

For 0x0 and 1x1 grids, any way of filling the grid will lead to unique hints, and will be locally solvable. For 2x2 and 3x3 grids, there are some hints that have multiple solutions, and thus cannot be solved by any system looking for a unique solution. All 2x2 and 3x3 nonograms with hints that lead to a unique solution are known to be locally solvable. It is only when we get to 4x4 grids that things become interesting, as there are hints that have a unique solution, but local logic is insufficient to find it. For example, the following hint has a unique solution, but local logic can only deduce what a few cells are before no more progress can be made:

```
   │1│ │ │ │
   │ │ │ │ │
   │1│1│1│3│
───┼─┼─┼─┼─┤
1 1│ │ │ │ │
───┼─┼─┼─┼─┤
  1│ │ │ │ │
───┼─┼─┼─┼─┤
1 1│ │ │ │ │
───┼─┼─┼─┼─┤
  2│ │ │ │ │
───┴─┴─┴─┴─┘
```

This code is incredibly slow to run for large sizes, as it looks at every nxm grid, of which there are 2^(n*m), and attempts to solve it. On a cheap laptop, 5x5 takes about 12 seconds, and 6x6 takes about 12 hours. My current estimate for 7x7 is that it will take more than 12 years.

## Results

The number of locally solvable nxm nonograms is documented in the table below. Since the table is symmetric, the upper triangle where n < m is omitted.

|     | m=0 | 1     | 2         | 3        | 4         | 5         | 6
|-----|-----|-------|-----------|----------|-----------|-----------|---
| n=0 | 1   |       |           |          |           |           |
| 1   | 1   | 2     |           |          |           |           |
| 2   | 1   | 4     | 14        |          |           |           |
| 3   | 1   | 8     | 52        | 384      |           |           |
| 4   | 1   | 16    | 208       | 3116     | 51234     |           |
| 5   | 1   | 32    | 810       | 24052    | 801832    | 24976511  |
| 6   | 1   | 64    | 3178      | 188042   | 12560410  | 781005372 | 48625108931
| 7   | 1   | 128   | 12418     | 1457710  | 195819250
| 8   | 1   | 256   | 48448     | 11284750
| 9   | 1   | 512   | 188994    | 87341874
| 10  | 1   | 1024  | 736786
| 11  | 1   | 2048  | 2872076
| 12  | 1   | 4096  | 11194600
| 13  | 1   | 8192  | 43631762
| 14  | 1   | 16384 | 170054426
| n   | 1   | 2ⁿ

## Dependencies

Dependencies are kept minimal. `cargo` is used to compile rust code. The rust config file `Cargo.toml` specifies any libraries used.

## Running

Building and running the project is done using `cargo`. `cargo build --release` will build the project, `cargo run --release` will run it, and `cargo clean` will clean up the temporary build files. `cargo run --release` will also build the project if it is out of date. The executable takes a size from command line arguments, one value for a square and two for a rectangle, and then prints the count to standard output. For example, `cargo run -- 4` will print `51234`, and `cargo run -- 2 5` will print `810`.

The program saves its progress when it is quit and when it finishes. The file name can be specified using the argument `-f <file>`, and defaults to `nonogram_data`. This file contains on separate lines:
* The width of the grid
* The height of the grid
* The number of grids attempted
* The number of grids solved
* The total time in milliseconds spent solving

If the specified width and height do not match what was saved, or the file is missing, the program will start from scratch.

## Rust Source Code

Rust code for the executable and libraries is contained in the `src` directory. Dependencies are managed by cargo, and can be seen in `Cargo.toml`.

## Contributors

Thanks to Laeeque for first suggesting the line by line approach to solving as the limitation that led to the number 24976511, and for introducing the term "local" for this kind of approach.

Thanks to [Paige](https://github.com/physical-memory-paige) for helping me learn Rust while I ported this project from Haskell to Rust.

Thanks to [mcpower](https://github.com/mcpower/mc-local-nonogram) for finding various optimisations, such as the idea of precomputing a lookup table for making progress on a row, and the use of SIMD parallelisation.

## Previous Work

(Pixelogic Weekly #14)[https://weekly.pixelogic.app/p/pixelogic-weekly-14]

(Bors, P.P. (Philippe), Analysis of Nonsimple Nonograms, Thesis Bachelor Informatica, LIACS, Leiden University, 2020.)[https://theses.liacs.nl/1853]

(Batenburg, Kees & Kosters, Walter. (2012). On the Difficulty of Nonograms. ICGA journal. 35. 195-205. 10.3233/ICG-2012-35402.)[https://www.researchgate.net/publication/290264363_On_the_Difficulty_of_Nonograms]