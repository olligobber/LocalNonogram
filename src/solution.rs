use itertools::Itertools;

use crate::line::Line;

// Structure for storing the solution to a nonogram
pub struct Solution {
	pub width: u8,
	pub height: u8,
	cols: Vec<Vec<bool>>,
	symmetries: Vec<u64>,
	pub num_symmetries: u8,
	row_sols: Vec<Line>,
	col_sols: Vec<Line>,
}

impl Solution {
	// Initialise the data structure with all false
	pub fn new(width: u8, height: u8) -> Solution {
		Solution {
			width,
			height,
			cols: vec![ vec![ false; usize::from(height) ]; usize::from(width) ],
			symmetries: vec![ 0; if width == height { 8 } else { 4 } ],
			num_symmetries: 1,
			row_sols: vec![ Line::blank(width); usize::from(height) ],
			col_sols: vec![ Line::blank(height); usize::from(width) ],
		}
	}

	// Load the grid with a particular index into memory
	pub fn load(&mut self, index: u64) {
		self.symmetries[0] = index;

		let mut loader = index;

		for x in 0..usize::from(self.width) {
			for y in 0..usize::from(self.height) {
				self.cols[x][y] = loader & 1 == 1;
				loader <<= 1;
			}
		}

		self.symmetries[1] = 0;
		for x in (0..usize::from(self.width)).rev() {
			for y in 0..usize::from(self.height) {
				self.symmetries[1] <<= 1;
				if self.cols[x][y] {
					self.symmetries[1] |= 1;
				}
			}
		}

		self.symmetries[2] = 0;
		for x in 0..usize::from(self.width) {
			for y in (0..usize::from(self.height)).rev() {
				self.symmetries[2] <<= 1;
				if self.cols[x][y] {
					self.symmetries[2] |= 1;
				}
			}
		}

		self.symmetries[3] = 0;
		for x in (0..usize::from(self.width)).rev() {
			for y in (0..usize::from(self.height)).rev() {
				self.symmetries[3] <<= 1;
				if self.cols[x][y] {
					self.symmetries[3] |= 1;
				}
			}
		}

		if self.width == self.height {

			self.symmetries[4] = 0;
			for y in 0..usize::from(self.height) {
				for x in 0..usize::from(self.width) {
					self.symmetries[4] <<= 1;
					if self.cols[x][y] {
						self.symmetries[4] |= 1;
					}
				}
			}

			self.symmetries[5] = 0;
			for y in (0..usize::from(self.height)).rev() {
				for x in 0..usize::from(self.width) {
					self.symmetries[5] <<= 1;
					if self.cols[x][y] {
						self.symmetries[5] |= 1;
					}
				}
			}

			self.symmetries[6] = 0;
			for y in 0..usize::from(self.height) {
				for x in (0..usize::from(self.width)).rev() {
					self.symmetries[6] <<= 1;
					if self.cols[x][y] {
						self.symmetries[6] |= 1;
					}
				}
			}

			self.symmetries[7] = 0;
			for y in (0..usize::from(self.height)).rev() {
				for x in (0..usize::from(self.width)).rev() {
					self.symmetries[7] <<= 1;
					if self.cols[x][y] {
						self.symmetries[7] |= 1;
					}
				}
			}

		}

		self.symmetries.sort();
		self.num_symmetries = u8::try_from(self.symmetries.iter().dedup().count()).unwrap();
	}

	// Get a representative of the symmetries of a grid
	pub fn symmetry_repr(&self) -> u64 {
		self.symmetries[0]
	}

	pub fn sols(&mut self) -> (&Vec<Line>, &Vec<Line>) {
		for x in 0..self.width {
			for y in 0..self.height {
				self.row_sols[usize::from(y)].set(x, self.cols[usize::from(x)][usize::from(y)]);
				self.col_sols[usize::from(x)].set(y, self.cols[usize::from(x)][usize::from(y)]);
			}
		}
		(&self.row_sols, &self.col_sols)
	}
}