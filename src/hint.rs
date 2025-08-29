use std::collections::VecDeque;

use crate::cell::Cell;
use crate::cell::Cell::*;

#[derive(Debug)]
pub struct Hint {
	pub hint: Vec<usize>,
}

impl Hint {
	pub fn progress(&self, state: &Vec<Cell>) -> Vec<Cell> {
		let size: usize = state.len();
		let mut result: Vec<Cell> = Vec::new();
		for line in HintLineIterator::new(state, self) {
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
			panic!("Contradiction!");
		}
		result
	}
}

#[derive(Clone)]
struct HintLineState {
	remaining_hints: VecDeque<usize>,
	line_so_far: VecDeque<bool>,
	remaining_knowledge: VecDeque<Cell>,
	current_hint: Option<usize>,
}

struct HintLineIterator {
	possibilities: VecDeque<HintLineState>,
}

impl HintLineIterator {
	fn new(knowledge: &Vec<Cell>, hints: &Hint) -> HintLineIterator {
		HintLineIterator {
			possibilities: VecDeque::from([HintLineState
				{ remaining_hints: VecDeque::from(hints.hint.clone())
				, line_so_far: VecDeque::new()
				, remaining_knowledge: VecDeque::from(knowledge.clone())
				, current_hint: None
				}
			])
		}
	}
}

impl Iterator for HintLineIterator {
	type Item = Vec<bool>;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			match self.possibilities.pop_back() {
				None => { return None }
				Some(mut state) => {
					match (state.current_hint, state.remaining_knowledge.pop_back()) {
						(None, None) => {
							if state.remaining_hints.is_empty() {
								return Some(state.line_so_far.into_iter().collect::<Vec<bool>>());
							}
						}
						(None, Some(Full)) => {
							match state.remaining_hints.pop_front() {
								None => {}
								Some(hint) => {
									state.line_so_far.push_back(true);
									state.current_hint = Some(hint-1);
									self.possibilities.push_back(state);
								}
							}
						}
						(None, Some(Empty)) => {
							state.line_so_far.push_back(false);
							self.possibilities.push_back(state);
						}
						(None, Some(Unknown)) => {
							let mut state2 = state.clone();
							state.remaining_knowledge.push_front(Empty);
							state2.remaining_knowledge.push_front(Full);
							self.possibilities.push_back(state2);
							self.possibilities.push_back(state);
						}
						(Some(0), None) => {
							state.current_hint = None;
							self.possibilities.push_back(state);
						}
						(Some(0), Some(Full)) => {}
						(Some(0), _) => {
							state.line_so_far.push_back(false);
							state.current_hint = None;
							self.possibilities.push_back(state);
						}
						(Some(_), None) => {}
						(Some(_), Some(Empty)) => {}
						(Some(n), _) => {
							state.line_so_far.push_back(true);
							state.current_hint = Some(n-1);
							self.possibilities.push_back(state);
						}
					}
				}
			}
		}
	}
}