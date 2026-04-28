use bitvec::prelude::*;

use crate::line::Line;

// A hint for a line, consisting a series of 1s the length of each block separated by 0s
#[derive(Debug)]
pub struct Hint {
	pub length: usize,
	pub contents: BitArray<[u16; 1], Lsb0>,
}

impl Hint {
	pub fn new(line: Line) -> Hint {
		let mut result : BitArray<[u16; 1], Lsb0> = BitArray::ZERO;
		let mut result_pos : usize = 0;
		let mut in_block : bool = false;
		for i in 0..line.length {
			if line.contents[i] {
				result.set(result_pos, true);
				result_pos += 1;
				in_block = true;
			} else if in_block {
				result_pos += 1;
				in_block = false;
			}
		}
		Hint { length: line.length, contents: result }
	}
}