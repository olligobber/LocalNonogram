use bitvec::prelude::*;

// Structure for storing a line of a grid
#[derive(Debug)]
pub struct Line {
	pub length: usize,
	pub contents: BitArray<[u16; 1]>,
}