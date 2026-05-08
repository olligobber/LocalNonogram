use bitvec::prelude::*;
use itertools::Itertools;

use crate::line::Line;

// A single solution's index
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct SolIndex {
	index: BitArray<[u64; 1], Lsb0>,
}

impl SolIndex {
	// A new index set to zero
	pub fn new() -> SolIndex {
		SolIndex { index: BitArray::ZERO }
	}

	// Load a number into the index
	pub fn load(&mut self, index: u64) {
		self.index.as_raw_mut_slice()[0] = index;
	}

	// Get the index as a number
	pub fn index(&self) -> u64 {
		self.index.into_inner()[0]
	}

	// Get a particular cell
	pub fn get_cell(&self, width: u8, x: u8, y: u8) -> bool {
		self.index[usize::from(y) * usize::from(width) + usize::from(x)]
	}

	// Set a particular cell
	pub fn set_cell(&mut self, width: u8, x: u8, y: u8, val: bool) {
		self.index.set(usize::from(y) * usize::from(width) + usize::from(x), val)
	}

}

// Structure for storing the solution to a nonogram
pub struct Solution {
	pub width: u8,
	pub height: u8,
	index: SolIndex,
	symmetries: Vec<SolIndex>,
	pub num_symmetries: u8,
	pub row_sols: Vec<Line>,
	pub col_sols: Vec<Line>,
}

impl Solution {
	// Initialise the data structure with all false
	pub fn new(width: u8, height: u8) -> Solution {
		Solution {
			width,
			height,
			index: SolIndex::new(),
			symmetries: vec![ SolIndex::new(); if width == height { 8 } else { 4 } ],
			num_symmetries: 1,
			row_sols: vec![ Line::blank(width); usize::from(height) ],
			col_sols: vec![ Line::blank(height); usize::from(width) ],
		}
	}

	// Load the grid with a particular index into memory
	pub fn load(&mut self, index: u64) {
		self.index.load(index);

		self.symmetries[0].load(index);

		for x in 0..self.width {
			for y in 0..self.height {
				let cell = self.index.get_cell(self.width, x, y);
				self.symmetries[1].set_cell(self.width, self.width - x - 1, y, cell);
				self.symmetries[2].set_cell(self.width, x, self.height - y - 1, cell);
				self.symmetries[3].set_cell(self.width, self.width - x - 1, self.height - y - 1, cell);
				if self.width == self.height {
					self.symmetries[4].set_cell(self.width, y, x, cell);
					self.symmetries[5].set_cell(self.width, self.height - y - 1, x, cell);
					self.symmetries[6].set_cell(self.width, y, self.width - x - 1, cell);
					self.symmetries[7].set_cell(self.width, self.height - y - 1, self.width - x - 1, cell);
				}
				self.row_sols[usize::from(y)].set(x, cell);
				self.col_sols[usize::from(x)].set(y, cell);
			}
		}
		self.symmetries.sort();
		self.num_symmetries = u8::try_from(self.symmetries.iter().dedup().count()).unwrap();
	}

	// Get a representative of the symmetries of a grid
	pub fn symmetry_repr(&self) -> u64 {
		self.symmetries[0].index()
	}
}