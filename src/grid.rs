// Structure for storing grids
pub struct Grid {
	width: usize,
	height: usize,
	pub rows: Vec<Vec<bool>>,
}

impl Grid {
	// Initialise the data structure with all false
	pub fn new(width: usize, height: usize) -> Grid {
		Grid {
			width,
			height,
			rows: vec![vec![false; width]; height],
		}
	}

	// Load the grid with a particular index into memory
	// If the index is out of range, a wrapped around version will be loaded
	// Returns true if the index was in range and false if it was out of range
	pub fn load(&mut self, index: u64) -> bool {
		let mut reader: u64 = index;
		for i in 0..self.height {
			for j in 0..self.width {
				self.rows[i][j] = reader & 1 == 0;
				reader >>= 1;
			}
		}
		reader == 0
	}
}