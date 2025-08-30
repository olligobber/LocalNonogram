use std::cell::RefCell;
use mempool::Pool;

use crate::cell::Cell;
use crate::cell::Cell::*;

// A single state for iterating over all possible solutions to a line
#[derive(Debug)]
struct HintLineState<'a> {
	mempool: &'a Pool<RefCell<Vec<bool>>>,
	remaining_hints: &'a [usize], // hints that have not been placed yet
	line_so_far: &'a RefCell<Vec<bool>>, // the line that has been placed
	remaining_knowledge: &'a [Cell], // cell knowledge that has not been look at yet
	current_hint: Option<usize>, // the amount of the current hint remaining
}

impl Clone for HintLineState<'_> {
	fn clone(&self) -> Self {
		let line_so_far = self.mempool.get();
		let mut v = line_so_far.borrow_mut();
		let w = self.line_so_far.borrow();
		v.clear();
		v.extend(w.iter());
		drop(v);
		drop(w);
		HintLineState
			{ mempool: self.mempool
			, remaining_hints: self.remaining_hints
			, line_so_far
			, remaining_knowledge: self.remaining_knowledge
			, current_hint: self.current_hint
			}
	}
}

impl HintLineState<'_> {
	// Get the next hint, removing it from remaining_hints
	fn pop_hint(&mut self) -> Option<usize> {
		match self.remaining_hints.split_first() {
			None => { None }
			Some((hint, rest)) => {
				self.remaining_hints = rest;
				Some(*hint)
			}
		}
	}

	// Get the next cell of knowledge, removing it from remaining_knowledge
	fn pop_knowledge(&mut self) -> Option<Cell> {
		match self.remaining_knowledge.split_first() {
			None => { None }
			Some((cell, rest)) => {
				self.remaining_knowledge = rest;
				Some(*cell)
			}
		}
	}
}

// An iterator over all possible solutions to a line, given its hint and current knowledge
// A stack of possible states that have yet to be explored
pub struct HintLineIterator<'a> {
	possibilities: Vec<HintLineState<'a>>,
}

impl HintLineIterator<'_> {
	pub fn new<'a>(knowledge: &'a [Cell], hints: &'a Vec<usize>, mempool: &'a Pool<RefCell<Vec<bool>>>)
		-> HintLineIterator<'a>
	{
		let line_so_far = mempool.get();
		let mut v = line_so_far.borrow_mut();
		v.clear();
		drop(v);
		HintLineIterator {
			possibilities: Vec::from([HintLineState
				{ mempool
				, remaining_hints: hints
				, line_so_far
				, remaining_knowledge: knowledge
				, current_hint: None
			}
			]),
		}
	}
}

impl Iterator for HintLineIterator<'_> {
	type Item = Vec<bool>;

	fn next(&mut self) -> Option<Self::Item> {
		// We will repeatedly pop off a state and either progress or resolve it
		// Until we run out of states, or we return a full solution
		loop {
			match self.possibilities.pop() {
				None => { return None }
				Some(mut state) => {
					match (state.current_hint, state.pop_knowledge()) {
						(None, None) => {
							if state.remaining_hints.is_empty() {
								return Some(state.line_so_far.take());
							}
						}
						(None, Some(Full)) => {
							match state.pop_hint() {
								None => {}
								Some(hint) => {
									let mut v = state.line_so_far.borrow_mut();
									v.push(true);
									drop(v);
									state.current_hint = Some(hint-1);
									self.possibilities.push(state);
								}
							}
						}
						(None, Some(Empty)) => {
							let mut v = state.line_so_far.borrow_mut();
							v.push(false);
							drop(v);
							self.possibilities.push(state);
						}
						(None, Some(Unknown)) => {
							match state.remaining_hints.split_first() {
								None => {
									let mut v = state.line_so_far.borrow_mut();
									v.push(false);
									drop(v);
									self.possibilities.push(state);
								}
								Some((hint, rest_h)) => {
									let mut state2 = state.clone();
									let mut v = state.line_so_far.borrow_mut();
									v.push(false);
									drop(v);
									let mut v = state2.line_so_far.borrow_mut();
									v.push(true);
									drop(v);
									state2.current_hint = Some(hint - 1);
									state2.remaining_hints = rest_h;
									self.possibilities.push(state2);
									self.possibilities.push(state);
								}
							}

						}
						(Some(0), None) => {
							state.current_hint = None;
							self.possibilities.push(state);
						}
						(Some(0), Some(Full)) => {}
						(Some(0), _) => {
							let mut v = state.line_so_far.borrow_mut();
							v.push(false);
							drop(v);
							state.current_hint = None;
							self.possibilities.push(state);
						}
						(Some(_), None) => {}
						(Some(_), Some(Empty)) => {}
						(Some(n), _) => {
							let mut v = state.line_so_far.borrow_mut();
							v.push(true);
							drop(v);
							state.current_hint = Some(n-1);
							self.possibilities.push(state);
						}
					}
				}
			}
		}
	}
}