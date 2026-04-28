use bitvec::prelude::*;

use crate::line::Line;

// Structure for storing grids
pub struct Grid {
	width: usize,
	height: usize,
	pub rows: Vec<Vec<bool>>,
}

impl Grid {
	// Initialise the data structure with all false
	pub fn new(width: usize, height: usize) -> Grid {
		Grid {
			width,
			height,
			rows: vec![vec![false; width]; height],
		}
	}

	// Load the grid with a particular index into memory
	// If the index is out of range, a wrapped around version will be loaded
	// Returns true if the index was in range and false if it was out of range
	pub fn load(&mut self, index: u64) -> bool {
		let mut reader: u64 = index;
		for i in 0..self.height {
			for j in 0..self.width {
				self.rows[i][j] = reader & 1 == 1;
				reader >>= 1;
			}
		}
		reader == 0
	}

	// Extract a row out of a grid
	pub fn get_row(&self, row: usize) -> Line {
		let mut result: BitArray<[u16; 1]> = BitArray::ZERO;
		for j in 0..self.width {
			result.set(j, self.rows[row][j]);
		}
		Line { length: self.width, contents: result }
	}

	// Extract a column out of a grid
	pub fn get_col(&self, col: usize) -> Line {
		let mut result: BitArray<[u16; 1]> = BitArray::ZERO;
		for i in 0..self.height {
			result.set(i, self.rows[i][col])
		}
		Line { length: self.height, contents: result }
	}

	// Replace a row in a grid
	pub fn set_row(&mut self, row: usize, data: Line) {
		assert_eq!(self.width, data.length);
		for j in 0..self.width {
			self.rows[row][j] = data.contents[j];
		}
	}

	// Replace a column in a grid
	pub fn set_col(&mut self, col: usize, data: Line) {
		assert_eq!(self.height, data.length);
		for i in 0..self.height {
			self.rows[i][col] = data.contents[i];
		}
	}
}