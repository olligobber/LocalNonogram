use bitvec::prelude::*;

// Structure for storing a line of a grid
#[derive(Debug, Clone)]
pub struct Line {
	pub length: u8,
	contents: BitArray<[u16; 1], Lsb0>,
}

impl Line {
	// Create a new line of all zeroes
	pub fn blank(length: u8) -> Line {
		Line { length, contents: BitArray::ZERO }
	}

	// Creat a new line with a given index
	pub fn from(length: u8, index: u16) -> Line {
		Line { length, contents: BitArray::new([index]) }
	}

	// Load a particular index into the line
	pub fn load(&mut self, index: u16) {
		self.contents.as_raw_mut_slice()[0] = index;
	}

	// Get the index of this line
	pub fn index(&self) -> u16 {
		self.contents.into_inner()[0]
	}

	// Set a particular cell of this line
	pub fn set(&mut self, position: u8, val: bool) {
		self.contents.set(usize::from(position), val)
	}

	// Get a particular cell of this line
	pub fn get(&self, position: u8) -> bool {
		self.contents[usize::from(position)]
	}
}