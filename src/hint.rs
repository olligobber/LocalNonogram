use crate::line::Line;

// A hint for a line, consisting a series of 1s the length of each block separated by 0s
#[derive(Debug)]
pub struct Hint {
	pub length: usize,
	pub hint: u16,
}

impl Hint {
	pub fn new(line: Line) -> Hint {
		let mut result : u16 = 0;
		let mut data = line.contents;
		for _ in 0..line.length {
			if data & 1 == 1 {
				result <<= 1;
				result |= 1;
			} else if result & 1 == 1 {
				result <<= 1;
			}
			data >>= 1;
		}
		if result & 1 == 0 {
			result >>= 1;
		}
		Hint { length: line.length, hint: result }
	}
}