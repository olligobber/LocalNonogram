use std::io;

mod cell;
mod hint_line_iter;
mod hint;

mod grid;
use grid::Grid;

mod solution;
use solution::Solution::*;

mod hints;
use hints::Hints;

fn main() {
	// Parse the input
	let mut input = String::new();

	io::stdin()
		.read_line(&mut input)
		.expect("Failed to read line");

	let mut words = input.split_whitespace();

	let width: usize = words.next().unwrap().parse::<usize>().unwrap();

	let height: usize =
		if let Some(height_str) = words.next() {
			height_str.parse::<usize>().unwrap()
		} else {
			width
		};

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