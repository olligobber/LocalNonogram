# LocalNonogram

The purpose of this project is to count the number of nonograms which are solvable with a particular solving system, which we will call **locally solvable** or solvable by **local logic**. This was inspired by [Pixelogic's "Every 5x5 Nonogram"](https://pixelogic.app/every-5x5-nonogram), which tries to get all 24976511 solvable 5x5 nonograms solved by a human. However, it is not clearly explained where this number of 5x5 nonograms came from. It is not simply the number of hints with a unique solution, as this number is documented by [OEIS sequence A242876](https://oeis.org/A242876), which says the number of 5x5 nonograms with a unique solution is 25309575, which differs from Pixelogic's number by 333064. Instead, this number comes from solving with local logic, explained in detail in the next paragraph. The number of locally solvable nonograms is not yet documented on the OEIS, but we know its first terms, for square grids of size 0 to 5, are 1, 2, 14, 384, 51234, 24976511. This project aims to find more terms for this and related sequences.

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

This code is incredibly slow to run for large sizes, as it looks at every nxm grid, of which there are 2^(n*m), determines its hints, and attempts to apply inefficient logic to solve it. On a cheap laptop, 4x4 takes about 0.3 seconds, and 5x5 takes about 5 minutes. My current estimate for 6x6 is that it will take 29 hours.

## Dependencies

Dependencies are kept minimal. `cargo` is used to compile rust code. The rust config file `Cargo.toml` specifies any libraries used.

## Running

Building and running the project is done using `cargo`. `cargo build --release` will build the project, `cargo run --release` will run it, and `cargo clean` will clean up the temporary build files. `cargo run --release` will also build the project if it is out of date. The executable takes a size from standard input, one value for a square and two for a rectangle, and then prints the count to standard output. For example, `cargo run <<< "4"` will print `51234`, and `cargo run <<< "2 5"` will print `810`.

## Rust Source Code

Haskell code for the executable and libraries is contained in the `src` directory. Dependencies are managed by cabal, and can be seen in `Cargo.toml`.