use bitvec::prelude::*;

use crate::line::Line;

// Structure for storing current knowledge about a nonogram
pub struct Knowledge {
	pub width: u8,
	pub height: u8,
	pub rows: Vec<Vec<bool>>,
}

impl Knowledge {
	// Initialise the data structure with all false
	pub fn new(width: u8, height: u8) -> Knowledge {
		Knowledge {
			width,
			height,
			rows: vec![vec![false; usize::from(width)]; usize::from(height)],
		}
	}

	// Extract a row out of a grid
	pub fn get_row(&self, row: u8) -> Line {
		let mut result: BitArray<[u16; 1], Lsb0> = BitArray::ZERO;
		for j in 0..usize::from(self.width) {
			result.set(j, self.rows[usize::from(row)][j]);
		}
		Line { length: self.width, contents: result }
	}

	// Extract a column out of a grid
	pub fn get_col(&self, col: u8) -> Line {
		let mut result: BitArray<[u16; 1], Lsb0> = BitArray::ZERO;
		for i in 0..usize::from(self.height) {
			result.set(i, self.rows[i][usize::from(col)])
		}
		Line { length: self.height, contents: result }
	}

	// Replace a row in a grid
	pub fn set_row(&mut self, row: u8, data: Line) {
		assert_eq!(self.width, data.length);
		for j in 0..usize::from(self.width) {
			self.rows[usize::from(row)][j] = data.contents[j];
		}
	}

	// Replace a column in a grid
	pub fn set_col(&mut self, col: u8, data: Line) {
		assert_eq!(self.height, data.length);
		for i in 0..usize::from(self.height) {
			self.rows[i][usize::from(col)] = data.contents[i];
		}
	}

	pub fn count_false(&self) -> u8 {
		let mut result: u8 = 0;
		for row in &self.rows {
			for cell in row {
				if !*cell {
					result += 1;
				}
			}
		}
		result
	}
}