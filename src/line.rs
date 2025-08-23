use crate::cell::Cell;

pub enum Line {
	Going {
		line: Vec<bool>,
		changeable: Vec<usize>,
	},
	Finished,
}

impl Line {
	pub fn new(template: &Vec<Cell>) -> Line {
		let mut line: Vec<bool> = Vec::new();
		let mut changeable: Vec<usize> = Vec::new();
		for i in 0..template.len() {
			match template[i] {
				Cell::Empty => { line.push(false); },
				Cell::Full => { line.push(true); },
				Cell::Unknown => {
					line.push(false);
					changeable.push(i);
				}
			}
		}
		Line::Going {
			line: line,
			changeable: changeable,
		}
	}

	pub fn next(&mut self) {
		use Line::*;
		match self {
			Going{line, changeable} => {
				if changeable.iter().all(|i| line[*i]) {
					*self = Finished;
					return;
				}
				for i in changeable {
					if line[*i] {
						line[*i] = false;
					} else {
						line[*i] = true;
						return;
					}
				}
			},
			Finished => {}
		}
	}
}