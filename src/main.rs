use clap::Parser;
use std::path::Path;
use std::fs;
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

mod hint;
mod line;

mod grid;
use grid::Grid;

mod solution;
use solution::Solution::*;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	/// Width of the grid
	width: usize,

	/// Height of the grid, same as width if not provided
	height: Option<usize>,

	/// Location of file for saving/loading progress
	#[arg(short, long, default_value_t = String::from("nonogram_data"))]
	file: String,
}

fn main() {
	let start_time = Instant::now();

	let args = Args::parse();

	let width: usize = args.width;

	let height: usize = args.height.unwrap_or(width);

	if width * height >= 64 {
		panic!("Solver cannot handle grids with area of 64 or more.")
	}

	if width >= 16 || height >= 16 {
		panic!("Solver cannot handle grids with width or height 16 or more.")
	}

	let path = Path::new(&args.file);

	let mut total_tried : u64 = 0;
	let mut total_solved : u64 = 0;
	let mut saved_time = Duration::ZERO;

	if let Ok(file) = fs::read_to_string(path) {
		let mut lines = file.lines();
		let stored_width = usize::from_str(lines.next().expect("Not enough lines in file")).expect("Invalid width in file");
		let stored_height = usize::from_str(lines.next().expect("Not enough lines in file")).expect("Invalid height in file");
		if width == stored_width && height == stored_height {
			total_tried = u64::from_str(lines.next().expect("Not enough lines in file")).expect("Invalid total tried in file");
			total_solved = u64::from_str(lines.next().expect("Not enough lines in file")).expect("Invalid total solved in file");
			saved_time = Duration::from_millis(u64::from_str(lines.next().expect("Not enough lines in file")).expect("Invalid time in file"));
		}
	}

	// Build storage for the grid
	let mut grid = Grid::new(width, height);

	// A variable that is set to true when it's time to quit
	let quit = Arc::new(AtomicBool::new(false));
	let q = quit.clone();

	ctrlc::set_handler(move || {
		q.store(true, Ordering::SeqCst);
	}).expect("Error setting Ctrl-C handler");

	// Loop over every grid, get its hints, solve it, and count how many it solved
	// Hints with multiple grids will be attempted multiple times, but can't be solved so won't count multiple times
	loop {
		if !grid.load(total_tried) {
			println!("{total_solved}");
			quit.store(true, Ordering::SeqCst);
		}
		if quit.load(Ordering::SeqCst) {
			let total_time = (saved_time + start_time.elapsed()).as_millis();
			fs::write(path, format!("{width}\n{height}\n{total_tried}\n{total_solved}\n{total_time}"))
				.expect("Failed to save progress");
			break;
		}
		// TODO new solver
		if solution == Solved { total_solved += 1 }
		total_tried += 1;
	}
}