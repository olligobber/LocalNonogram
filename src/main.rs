use clap::Parser;

mod cell;
mod hint_line_iter;
mod hint;

mod grid;
use grid::Grid;

mod solution;
use solution::Solution::*;

mod hints;
use hints::Hints;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// Width of the grid
	width: usize,

	/// Height of the grid, same as width if not provided
	height: Option<usize>,
}

fn main() {
	let args = Args::parse();

	let width: usize = args.width;

	let height: usize = args.height.unwrap_or(width);

	// Build an iterator and counter for the number solved
	let mut grid = Grid::new(width, height);
	let mut total_solved : u64 = 0;

	// Loop over every grid, get its hints, solve it, and count how many it solved
	// Hints with multiple grids will be attempted multiple times, but can't be solved so won't count multiple times
	while let Grid::Going {ref rows} = grid {
		let hints = Hints::new(rows);
		let solution = hints.solve();
		if solution == Contradiction { panic!("Contradiction!") }
		if solution == Solved { total_solved += 1; }
		grid.next();
	}

	println!("{total_solved}");

}