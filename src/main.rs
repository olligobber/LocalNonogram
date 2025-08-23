use std::io;

mod grid;
use grid::Grid;

#[derive(PartialEq, Eq, Debug)]
enum Solution {
	Solved,
	Contradiction,
	Unsolved,
}

use Solution::*;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Cell {
	Unknown,
	Full,
	Empty,
}

use Cell::*;

type Size = usize;

#[derive(Debug)]
struct Hint (Vec<Size>);

impl Hint {
	fn possibilities(&self, size: Size) -> Vec<Vec<bool>> {
		if self.0.is_empty() {
			vec![vec![false; size.into()]]
		} else if size == 0 {
			vec![]
		} else {
			let head : Size = self.0[0];
			let mut tail : Vec<Size> = Vec::new();
			for i in 1..self.0.len() {
				tail.push(self.0[i])
			}
			if size < head {
				return vec![];
			} else if size == head {
				if tail.is_empty() {
					return vec![vec![true; size.into()]];
				} else {
					return vec![];
				}
			}
			let mut results: Vec<Vec<bool>> = Vec::new();
			for i in Hint(tail).possibilities(size - head - 1).iter_mut() {
				let mut result = vec![true; head.into()];
				result.push(false);
				result.append(i);
				results.push(result);
			}
			for i in self.possibilities(size - 1).iter_mut() {
				let mut result = vec![false];
				result.append(i);
				results.push(result);
			}
			return results;
		}
	}
	fn progress(&self, state: &Vec<Cell>) -> Vec<Cell> {
		let size: Size = state.len();
		let possibilities =
			self.possibilities(size).into_iter().filter(|i| {
				let mut compatible : bool = true;
				for j in 0..size {
					if i[j] && state[j] == Empty { compatible = false; break };
					if !i[j] && state[j] == Full { compatible = false; break };
				}
				compatible
			});
		let mut result: Vec<Cell> = Vec::new();
		for i in possibilities {
			if result.is_empty() {
				for j in 0..size {
					result.push(if i[j] { Full } else { Empty });
				}
			} else {
				for j in 0..size {
					if i[j] && result[j] == Empty { result[j] = Unknown };
					if !i[j] && result[j] == Full { result[j] = Unknown };
				}
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
			let mut this_hint : Vec<Size> = Vec::new();
			let mut latest_block : Size = 0;
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
			let mut this_hint : Vec<Size> = Vec::new();
			let mut latest_block : Size = 0;
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

	fn width(&self) -> Size {
		self.column_hints.len()
	}

	fn height(&self) -> Size {
		self.row_hints.len()
	}

	fn solve(&self) -> Solution {
		let mut grid: Vec<Vec<Cell>> = Vec::new();
		for _ in 0 .. self.width() {
			let mut new_line : Vec<Cell> = Vec::new();
			for _ in 0 .. self.height() {
				new_line.push(Unknown);
			}
			grid.push(new_line);
		}
		loop {
			let mut num_unknowns: Size = 0;
			for i in grid.iter() {
				for j in i {
					if *j == Unknown { num_unknowns+= 1 };
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
			let mut new_unknowns: Size = 0;
			for i in grid.iter() {
				for j in i {
					if *j == Unknown { new_unknowns+= 1 };
				}
			}
			if new_unknowns == num_unknowns { break }
		}
		let mut num_unknowns: Size = 0;
		for i in grid {
			for j in i {
				if j == Unknown { num_unknowns+= 1 };
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

	let width: Size = words.next().unwrap().parse::<Size>().unwrap();

	let height: Size =
		if let Some(height_str) = words.next() {
			height_str.parse::<Size>().unwrap()
		} else {
			width
		};

	let mut grid = Grid::new(width, height);
	let mut total_solved : u64 = 0;

	loop {
		match grid {
			Grid::going { ref rows } => {
				let hints = Hints::new(rows);
				let solution = hints.solve();
				if solution == Solved { total_solved += 1; }
				grid.next();
			},
			Grid::finished => { break }
		}
	}

	println!("{total_solved}");

}