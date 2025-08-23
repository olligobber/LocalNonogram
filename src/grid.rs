pub enum Grid {
	going {
		rows: Vec<Vec<bool>>,
	},
	finished,
}

impl Grid {
	pub fn new(width: usize, height: usize) -> Grid {
		Grid::going {
			rows: vec![vec![false; width]; height],
		}
	}
	pub fn next(&mut self) {
		use Grid::*;
		match self {
			going{rows} => {
				let mut incremented: bool = false;
				let mut no_more: bool = false;
				for i in (0..rows.len()).rev() {
					for j in (0..rows[i].len()).rev() {
						if rows[i][j] {
							rows[i][j] = false;
							if i == 0 && j == 0 {
								no_more = true;
							}
						} else {
							rows[i][j] = true;
							incremented = true;
							break;
						}
					}
					if incremented {
						break;
					}
				}
				if no_more {
					*self = finished;
				}
			},
			finished => {}
		}
	}
}