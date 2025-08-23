#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Cell {
	Unknown,
	Full,
	Empty,
}

use Cell::*;

impl Cell {
	pub fn compatible(self, other: bool) -> bool {
		if other && self == Empty { return false }
		if !other && self == Full { return false }
		return true
	}

	pub fn update(&mut self, other: bool) {
		if other && *self == Empty { *self = Unknown }
		if !other && *self == Full { *self = Unknown }
	}

	pub fn from_bool(from: bool) -> Cell {
		if from { Full } else { Empty }
	}
}