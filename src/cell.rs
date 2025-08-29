// Knowledge about a cell when solving a nonogram
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Cell {
	Unknown, // We know nothing about the cell
	Full, // The cell is definitely full/true
	Empty, // The cell is definitely empty/false
}

use Cell::*;

impl Cell {
	// Given a possible value for a cell, weaken our knowledge about it to account for that
	pub fn update(&mut self, other: bool) {
		if other && *self == Empty { *self = Unknown }
		if !other && *self == Full { *self = Unknown }
	}

	// Convert an actual cell value to its corresponding strong knowledge
	pub fn from_bool(from: bool) -> Cell {
		if from { Full } else { Empty }
	}
}