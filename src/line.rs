use bitvec::prelude::*;

// Structure for storing a line of a grid
#[derive(Debug)]
pub struct Line {
	pub length: usize,
	pub contents: BitArray<[u16; 1]>,
}

impl Line {
	pub fn load(length: usize, index: u16) -> Line {
		Line { length, contents: BitArray::new([index]) }
	}
}