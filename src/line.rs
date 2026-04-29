use bitvec::prelude::*;

// Structure for storing a line of a grid
#[derive(Debug)]
pub struct Line {
	pub length: u8,
	pub contents: BitArray<[u16; 1], Lsb0>,
}

impl Line {
	pub fn load(length: u8, index: u16) -> Line {
		Line { length, contents: BitArray::new([index]) }
	}
}