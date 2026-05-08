use bitvec::prelude::*;
use itertools::Itertools;

use crate::line::Line;

// Structure for storing grids
pub struct Grid {
	pub width: u8,
	pub height: u8,
	pub rows: Vec<Vec<bool>>,
}

impl Grid {
	// Initialise the data structure with all false
	pub fn new(width: u8, height: u8) -> Grid {
		Grid {
			width,
			height,
			rows: vec![vec![false; usize::from(width)]; usize::from(height)],
		}
	}

	// Load the grid with a particular index into memory
	// If the index is out of range, a wrapped around version will be loaded
	// Returns true if the index was in range and false if it was out of range
	pub fn load(&mut self, index: u64) -> bool {
		let mut reader: u64 = index;
		for i in 0..usize::from(self.height) {
			for j in 0..usize::from(self.width) {
				self.rows[i][j] = reader & 1 == 1;
				reader >>= 1;
			}
		}
		reader == 0
	}

	// Get the index of the current grid
	pub fn index(&self) -> u64 {
		let mut result: u64 = 0;
		for i in (0..usize::from(self.height)).rev() {
			for j in (0..usize::from(self.width)).rev() {
				result <<= 1;
				if self.rows[i][j] { result |= 1 };
			}
		}
		result
	}

	// Get the list of indices of all rotations and reflections of the current grid
	pub fn symmetries(&self) -> Vec<u64> {
		let mut result = vec![self.index()];

		// Reflect horizontally
		let mut hor: u64 = 0;
		for i in 0..usize::from(self.height) {
			for j in (0..usize::from(self.width)).rev() {
				hor <<= 1;
				if self.rows[i][j] { hor |= 1 };
			}
		}
		result.push(hor);

		// Reflect vertically
		let mut vert: u64 = 0;
		for i in (0..usize::from(self.height)).rev() {
			for j in 0..usize::from(self.width) {
				vert <<= 1;
				if self.rows[i][j] { vert |= 1 };
			}
		}
		result.push(vert);

		// Rotate half a turn
		let mut half: u64 = 0;
		for i in 0..usize::from(self.height) {
			for j in 0..usize::from(self.width) {
				half <<= 1;
				if self.rows[i][j] { half |= 1 };
			}
		}
		result.push(half);

		// Transformations that only work for square grids
		if self.height == self.width {
			// Ascending diagonal reflection
			let mut asc: u64 = 0;
			for j in (0..usize::from(self.width)).rev() {
				for i in (0..usize::from(self.height)).rev() {
					asc <<= 1;
					if self.rows[i][j] { asc |= 1 };
				}
			}
			result.push(asc);

			// Descending diagonal reflection
			let mut desc: u64 = 0;
			for j in 0..usize::from(self.width) {
				for i in 0..usize::from(self.height) {
					desc <<= 1;
					if self.rows[i][j] { desc |= 1 };
				}
			}
			result.push(desc);

			// Clockwise rotation
			let mut clock: u64 = 0;
			for j in (0..usize::from(self.width)).rev() {
				for i in 0..usize::from(self.height) {
					clock <<= 1;
					if self.rows[i][j] { clock |= 1 };
				}
			}
			result.push(clock);

			// Anticlockwise rotation
			let mut anti: u64 = 0;
			for j in 0..usize::from(self.width) {
				for i in (0..usize::from(self.height)).rev() {
					anti <<= 1;
					if self.rows[i][j] { anti |= 1 };
				}
			}
			result.push(anti);
		}

		result
	}

	// Get the number of distinct symmetries of a grid
	pub fn num_symmetries(&self) -> u8 {
		u8::try_from(self.symmetries().iter().unique().count()).unwrap()
	}

	// Get a representative of the symmetries of a grid
	pub fn symmetry_repr(&self) -> u64 {
		*self.symmetries().iter().min().unwrap()
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