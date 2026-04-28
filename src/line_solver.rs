use crate::line::Line;
use crate::hint::Hint;

pub struct LineSolver {
	length: usize,
	cache: Vec<u16>,
}

impl LineSolver {
	pub fn new(length: usize) -> LineSolver {
		let num_lines : u16 = u16::pow(2, u32::try_from(length).unwrap());

		let mut result : Vec<u16> = vec![0; usize::from(num_lines) * usize::from(num_lines)];

		let mut hint_collector : Vec<Vec<u16>> = vec![vec![]; usize::from(num_lines)];

		for line_index in 0..num_lines {
			let line = Line::load(length, line_index);
			let hint = Hint::new(line);
			let hint_index = hint.contents.into_inner()[0];
			hint_collector[usize::from(hint_index)].push(line_index);
		}

		// TODO loop over each line and knowledge and add its new knowledge to result

		LineSolver { length, cache: result }
	}

	// TODO function to update knowledge using cache
}