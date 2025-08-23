use std::io;

mod cell;
use cell::Cell;

mod grid;
use grid::Grid;

mod line;
use line::Line;

#[derive(PartialEq, Eq, Debug)]
enum Solution {
	Solved,
	Contradiction,
	Unsolved,
}

use Solution::*;

#[derive(Debug)]
struct Hint (Vec<usize>);

impl Hint {
	fn compatible(&self, line: &Vec<bool>) -> bool {
		let mut i: usize = 0;
		let mut count: usize = 0;
		for j in line {
			if *j {
				if i >= self.0.len() {
					return false;
				}
				count += 1;
			} else {
				if count == 0 { continue }
				if count != self.0[i] { return false }
				i += 1;
				count = 0;
			}
		}
		if count == 0 && i == self.0.len() { return true }
		if i != self.0.len() - 1 { return false }
		if count != self.0[i] { return false }
		return true
	}

	fn progress(&self, state: &Vec<Cell>) -> Vec<Cell> {
		let size: usize = state.len();
		let mut possibilities = Line::new(state);
		let mut result: Vec<Cell> = Vec::new();
		loop {
			match possibilities {
				Line::Going {ref line, changeable: _} => {
					if !self.compatible(line) {
						possibilities.next();
						continue;
					}
					if result.is_empty() {
						for i in line {
							result.push(Cell::from_bool(*i));
						}
					} else {
						for i in 0..size {
							result[i].update(line[i]);
						}
					}
					possibilities.next();
				}
				Line::Finished => { break }
			}
		}

		if result.is_empty() {
			panic!("Contradiction!");
		}
		result
	}
}

#[derive(Debug)]
struct Hints {
	row_hints: Vec<Hint>,
	column_hints: Vec<Hint>,
}

impl Hints {
	fn new(grid: &Vec<Vec<bool>>) -> Hints {
		let mut row_hints : Vec<Hint> = Vec::new();
		for row in grid {
			let mut this_hint : Vec<usize> = Vec::new();
			let mut latest_block : usize = 0;
			for i in row {
				if *i {
					latest_block += 1;
				} else if latest_block > 0 {
					this_hint.push(latest_block.clone());
					latest_block = 0;
				}
			}
			if latest_block > 0 {
				this_hint.push(latest_block);
			}
			row_hints.push(Hint(this_hint));
		}
		let mut column_hints : Vec<Hint> = Vec::new();
		for col in 0..grid.first().unwrap().len() {
			let mut this_hint : Vec<usize> = Vec::new();
			let mut latest_block : usize = 0;
			for i in grid {
				if i[col] {
					latest_block += 1;
				} else if latest_block > 0 {
					this_hint.push(latest_block.clone());
					latest_block = 0;
				}
			}
			if latest_block > 0 {
				this_hint.push(latest_block);
			}
			column_hints.push(Hint(this_hint));
		}
		Hints{row_hints: row_hints, column_hints: column_hints}
	}

	fn width(&self) -> usize {
		self.column_hints.len()
	}

	fn height(&self) -> usize {
		self.row_hints.len()
	}

	fn solve(&self) -> Solution {
		let mut grid: Vec<Vec<Cell>> = Vec::new();
		for _ in 0 .. self.width() {
			let mut new_line : Vec<Cell> = Vec::new();
			for _ in 0 .. self.height() {
				new_line.push(Cell::Unknown);
			}
			grid.push(new_line);
		}
		loop {
			let mut num_unknowns: usize = 0;
			for i in grid.iter() {
				for j in i {
					if *j == Cell::Unknown { num_unknowns+= 1 };
				}
			}
			for column in 0 .. self.width() {
				grid[column] = self.column_hints[column].progress(&grid[column]);
			}
			for row in 0 .. self.height() {
				let mut row_state: Vec<Cell> = Vec::new();
				for i in 0..self.width() {
					row_state.push(grid[i][row]);
				}
				let new_row = self.row_hints[row].progress(&row_state);
				for i in 0..self.width() {
					grid[i][row] = new_row[i];
				}
			}
			let mut new_unknowns: usize = 0;
			for i in grid.iter() {
				for j in i {
					if *j == Cell::Unknown { new_unknowns+= 1 };
				}
			}
			if new_unknowns == num_unknowns { break }
		}
		let mut num_unknowns: usize = 0;
		for i in grid {
			for j in i {
				if j == Cell::Unknown { num_unknowns+= 1 };
			}
		}
		if num_unknowns == 0 { Solved } else { Unsolved }
	}
}

fn main() {
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

	let mut grid = Grid::new(width, height);
	let mut total_solved : u64 = 0;

	loop {
		match grid {
			Grid::Going { ref rows } => {
				let hints = Hints::new(rows);
				let solution = hints.solve();
				if solution == Solved { total_solved += 1; }
				grid.next();
			},
			Grid::Finished => { break }
		}
	}

	println!("{total_solved}");

}