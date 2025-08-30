use std::cell::RefCell;
use mempool::Pool;

use crate::cell::Cell;
use crate::hint_line_iter::HintLineIterator;

// A hint for a line, consisting of the size of each segment in order
#[derive(Debug)]
pub struct Hint {
	pub hint: Vec<usize>,
}

impl Hint {
	// Update a line given its hint and current knowledge
	// Returns None if it gets a contradiction
	// Loops over every possible solution, combining them into the new knowledge of that line
	pub fn progress(&self, state: &[Cell], mempool: &mut Pool<RefCell<Vec<bool>>>)
		-> Option<Vec<Cell>>
	{
		let size: usize = state.len();
		let mut result: Vec<Cell> = Vec::new();
		for line in HintLineIterator::new(state, &self.hint, mempool) {
			if result.is_empty() {
				for i in line {
					result.push(Cell::from_bool(i));
				}
			} else {
				for i in 0..size {
					result[i].update(line[i]);
				}
			}
		}

		if result.is_empty() {
			return None
		}
		Some(result)
	}
}
