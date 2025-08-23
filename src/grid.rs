pub enum Grid {
	Going {
		rows: Vec<Vec<bool>>,
	},
	Finished,
}

impl Grid {
	pub fn new(width: usize, height: usize) -> Grid {
		Grid::Going {
			rows: vec![vec![false; width]; height],
		}
	}
	pub fn next(&mut self) {
		use Grid::*;
		match self {
			Going{rows} => {
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
							return;
						}
					}
				}
				if no_more {
					*self = Finished;
				}
			},
			Finished => {}
		}
	}
}