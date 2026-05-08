use crate::line::Line;

// Structure for storing current knowledge about a nonogram
pub struct Knowledge {
	pub width: u8,
	pub height: u8,
	pub rows: Vec<Vec<bool>>,
}

impl Knowledge {
	// Initialise the data structure with all false
	pub fn new(width: u8, height: u8) -> Knowledge {
		Knowledge {
			width,
			height,
			rows: vec![vec![false; usize::from(width)]; usize::from(height)],
		}
	}

	// Reset knowledge to false
	pub fn reset(&mut self) {
		for i in 0..usize::from(self.height) {
			for j in 0..usize::from(self.width) {
				self.rows[i][j] = false;
			}
		}
	}

	// Extract a row out of a grid
	pub fn get_row(&self, row: u8) -> Line {
		let mut result = Line::blank(self.width);
		for j in 0..self.width {
			result.set(j, self.rows[usize::from(row)][usize::from(j)]);
		}
		result
	}

	// Extract a column out of a grid
	pub fn get_col(&self, col: u8) -> Line {
		let mut result = Line::blank(self.height);
		for i in 0..self.height {
			result.set(i, self.rows[usize::from(i)][usize::from(col)])
		}
		result
	}

	// Replace a row in a grid
	pub fn set_row(&mut self, row: u8, data: Line) {
		assert_eq!(self.width, data.length);
		for j in 0..self.width {
			self.rows[usize::from(row)][usize::from(j)] = data.get(j);
		}
	}

	// Replace a column in a grid
	pub fn set_col(&mut self, col: u8, data: Line) {
		assert_eq!(self.height, data.length);
		for i in 0..self.height {
			self.rows[usize::from(i)][usize::from(col)] = data.get(i);
		}
	}

	pub fn count_false(&self) -> u8 {
		let mut result: u8 = 0;
		for row in &self.rows {
			for cell in row {
				if !*cell {
					result += 1;
				}
			}
		}
		result
	}
}