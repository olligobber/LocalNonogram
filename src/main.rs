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

	// /// Location of file for saving/loading progress
	// #[arg(short, long, default_value_t = String::from("nonogram_data"))]
	// file: String,
}

fn main() {
	let args = Args::parse();

	let width: usize = args.width;

	let height: usize = args.height.unwrap_or(width);

	if width * height >= 64 {
		panic!("Solver cannot handle grids with area of 64 or more.")
	}

	// Build storage for the grid, a count of the number of grids attempted, and a count of the number solved
	let mut grid = Grid::new(width, height);
	let mut total_tried : u64 = 0;
	let mut total_solved : u64 = 0;

	// Loop over every grid, get its hints, solve it, and count how many it solved
	// Hints with multiple grids will be attempted multiple times, but can't be solved so won't count multiple times
	loop {
		if !grid.load(total_tried) {
			break
		}
		let hints = Hints::new(&grid.rows);
		let solution = hints.solve();
		if solution == Contradiction { panic!("Contradiction!") }
		if solution == Solved { total_solved += 1 }
		total_tried += 1;
	}

	println!("{total_solved}");

}