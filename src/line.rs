pub enum Line {
	Going {
		vals: Vec<bool>,
	},
	Finished,
}

impl Line {
	pub fn new(length: usize) -> Line {
		Line::Going {
			vals: vec![false; length],
		}
	}
	pub fn next(&mut self) {
		use Line::*;
		match self {
			Going{vals} => {
				let mut no_more: bool = false;
				for i in (0..vals.len()).rev() {
					if vals[i] {
						vals[i] = false;
						if i == 0 {
							no_more = true;
						}
					} else {
						vals[i] = true;
						return;
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