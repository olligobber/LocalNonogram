// The solution to a grid
#[derive(PartialEq, Eq, Debug)]
pub enum Solution {
	Solved,
	Contradiction,
	Unsolved,
}