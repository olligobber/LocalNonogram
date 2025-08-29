use crate::cell::Cell;
use crate::hint_line_iter::HintLineIterator;

#[derive(Debug)]
pub struct Hint {
	pub hint: Vec<usize>,
}

impl Hint {
	pub fn progress(&self, state: &[Cell]) -> Option<Vec<Cell>> {
		// println!("Progressing with hint {self:?} and state {state:?}");
		let size: usize = state.len();
		let mut result: Vec<Cell> = Vec::new();
		for line in HintLineIterator::new(state, &self.hint) {
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
		// println!("Finished line with new state {result:?}");
		Some(result)
	}
}
