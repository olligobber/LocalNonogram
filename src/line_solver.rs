use crate::line::Line;
use crate::hint::Hint;

#[derive(Debug)]
pub struct LineSolver {
	length: u8,
	cache: Vec<u16>,
}

impl LineSolver {
	pub fn new(length: u8) -> LineSolver {
		let num_lines : u16 = u16::pow(2, u32::from(length));

		let mut result : Vec<u16> = vec![0; usize::from(num_lines) * usize::from(num_lines)];

		let mut hint_collector : Vec<Vec<u16>> = vec![vec![]; usize::from(num_lines)];

		for line_index in 0..num_lines {
			let line = Line::load(length, line_index);
			let hint = Hint::new(line);
			let hint_index = hint.contents.into_inner()[0];
			hint_collector[usize::from(hint_index)].push(line_index);
		}

		for line_index in 0..num_lines {
			for knowledge in 0..num_lines {
				let line = Line::load(length, line_index);
				let hint = Hint::new(line);
				let hint_index = hint.contents.into_inner()[0];
				let mut new_knowledge: u16 = num_lines - 1;
				// For each solution that matches the hint
				for l in &hint_collector[usize::from(hint_index)] {
					// Ignore lines that don't match the current knowledge
					if l & knowledge != line_index & knowledge {
						continue;
					}
					// Get the bits that match the actual solution
					let ok_bits = !(line_index ^ l);
					// Keep only those bits in the new knowledge
					new_knowledge &= ok_bits;
				}
				let cache_index = u32::from(line_index) << length | u32::from(knowledge);
				result[usize::try_from(cache_index).unwrap()] = new_knowledge;
			}
		}

		LineSolver { length, cache: result }
	}

	pub fn progress(&self, solution: &Line, knowledge: &Line) -> Line {
		assert_eq!(self.length, solution.length);
		assert_eq!(self.length, knowledge.length);
		let solution_index = solution.contents.into_inner()[0];
		let knowledge_index = knowledge.contents.into_inner()[0];
		let cache_index = u32::from(solution_index) << self.length | u32::from(knowledge_index);
		let result_index = self.cache[usize::try_from(cache_index).unwrap()];
		Line::load(self.length, result_index)
	}
}