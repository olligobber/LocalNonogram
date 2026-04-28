use crate::cell::Cell;
use crate::hint::Hint;
use crate::solution::Solution;
use crate::solution::Solution::*;

// Hints for a whole puzzle grid
#[derive(Debug)]
pub struct Hints {
	row_hints: Vec<Hint>,
	column_hints: Vec<Hint>,
}

impl Hints {
	pub fn new(grid: &Vec<Vec<bool>>) -> Hints {
		let mut row_hints : Vec<Hint> = Vec::new();
		for row in grid {
			let mut this_hint : Vec<usize> = Vec::new();
			let mut latest_block : usize = 0;
			for i in row {
				if *i {
					latest_block += 1;
				} else if latest_block > 0 {
					this_hint.push(latest_block);
					latest_block = 0;
				}
			}
			if latest_block > 0 {
				this_hint.push(latest_block);
			}
			row_hints.push(Hint { hint: this_hint });
		}
		let mut column_hints : Vec<Hint> = Vec::new();
		for col in 0..grid.first().unwrap().len() {
			let mut this_hint : Vec<usize> = Vec::new();
			let mut latest_block : usize = 0;
			for i in grid {
				if i[col] {
					latest_block += 1;
				} else if latest_block > 0 {
					this_hint.push(latest_block);
					latest_block = 0;
				}
			}
			if latest_block > 0 {
				this_hint.push(latest_block);
			}
			column_hints.push(Hint { hint: this_hint });
		}
		Hints{row_hints, column_hints}
	}

	pub fn width(&self) -> usize {
		self.column_hints.len()
	}

	pub fn height(&self) -> usize {
		self.row_hints.len()
	}

	pub fn solve(&self) -> Solution {
		// Build a grid of all unknowns
		let mut grid: Vec<Vec<Cell>> = Vec::new();
		for _ in 0 .. self.width() {
			let mut new_line : Vec<Cell> = Vec::new();
			for _ in 0 .. self.height() {
				new_line.push(Cell::Unknown);
			}
			grid.push(new_line);
		}
		// Repeatedly update every row and column until either
		// * We find a contradiction
		// * There are no unknown cells left
		// * Nothing changes after updating every row and column once
		loop {
			let mut num_unknowns: usize = 0;
			for i in grid.iter() {
				for j in i {
					if *j == Cell::Unknown { num_unknowns+= 1 };
				}
			}
			for column in 0 .. self.width() {
				match self.column_hints[column].progress(&grid[column]) {
					None => { return Contradiction }
					Some(new_col) => { grid[column] = new_col }
				}
			}
			for row in 0 .. self.height() {
				let mut row_state: Vec<Cell> = Vec::new();
				for i in 0..self.width() {
					row_state.push(grid[i][row]);
				}
				match self.row_hints[row].progress(&row_state) {
					None => { return Contradiction }
					Some(new_row) => {
						for i in 0..self.width() {
							grid[i][row] = new_row[i];
						}
					}
				}
			}
			let mut new_unknowns: usize = 0;
			for i in grid.iter() {
				for j in i {
					if *j == Cell::Unknown { new_unknowns+= 1 };
				}
			}
			if new_unknowns == num_unknowns { break }
			if new_unknowns == 0 { break }
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