use crate::line_solver::LineSolver;
use crate::grid::Grid;
use crate::solution::Solution;
use crate::line::Line;

#[derive(Debug)]
pub struct GridSolver {
	width: u8,
	height: u8,
	row_solver: LineSolver,
	col_solver: Option<LineSolver>,
}

impl GridSolver {
	pub fn new(width: u8, height: u8) -> GridSolver {
		let row_solver = LineSolver::new(width);
		let col_solver =
			if width == height {
				Option::None
			} else {
				Option::Some(LineSolver::new(height))
			};
		GridSolver { width, height, row_solver, col_solver }
	}

	pub fn solve(&self, grid: &Grid) -> Solution {
		assert_eq!(self.width, grid.width);
		assert_eq!(self.height, grid.height);
		let row_solver = &self.row_solver;
		let col_solver = self.col_solver.as_ref().unwrap_or(row_solver);
		let row_sols: Vec<Line> = (0..self.height).map(|j| grid.get_row(j)).collect();
		let col_sols: Vec<Line> = (0..self.width).map(|i| grid.get_col(i)).collect();
		let mut knowledge = Grid::new(self.width, self.height);
		loop {
			let old_unknowns = knowledge.count_false();
			for j in 0..self.height {
				let old_row_knowledge = knowledge.get_row(j);
				let row_sol = &row_sols[usize::from(j)];
				let new_row_knowledge = row_solver.progress(row_sol, &old_row_knowledge);
				knowledge.set_row(j, new_row_knowledge);
			}
			for i in 0..self.width {
				let old_col_knowledge = knowledge.get_col(i);
				let col_sol = &col_sols[usize::from(i)];
				let new_col_knowledge = col_solver.progress(col_sol, &old_col_knowledge);
				knowledge.set_col(i, new_col_knowledge);
			}
			let new_unknowns = knowledge.count_false();
			if new_unknowns == 0 {
				return Solution::Solved;
			}
			if new_unknowns == old_unknowns {
				return Solution::Unsolved;
			}
		}
	}
}