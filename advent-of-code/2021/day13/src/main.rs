use std::collections::HashSet;

#[derive(Hash, Debug, Eq, Copy, PartialOrd, Ord, Clone, PartialEq)]
struct Point {
	x: usize,
	y: usize,
}

impl Point {
	fn parse(s: &str) -> Self {
		let parts: Vec<&str> = s.split(',').collect();
		Self {
			x: parts[0].parse::<usize>().unwrap(),
			y: parts[1].parse::<usize>().unwrap(),
		}
	}
}

#[derive(Hash, Debug, Eq, Copy, Clone, PartialEq)]
struct Fold {
	direction: char,
	value: usize,
}

impl Fold {
	fn parse(s: &str) -> Self {
		let parts: Vec<&str> = s.split(' ').collect();
		let parts: Vec<&str> = parts[2].split('=').collect();
		Self {
			direction: parts[0].chars().next().unwrap(),
			value: parts[1].parse::<usize>().unwrap(),
		}
	}
}

fn main() {
	let input = include_str!("../input")
		.split("\n\n")
		.collect::<Vec<&str>>();
	let mut points = input[0]
		.lines()
		.map(Point::parse)
		.collect::<HashSet<Point>>();
	let folds = input[1].lines().map(Fold::parse).collect::<Vec<Fold>>();

	let mut first = true;
	for fold in folds.iter() {
		points = points.iter().fold(HashSet::new(), |mut acc, p| {
			// For each point, if it's over the fold line, we replace
			// it with a new point that's on the other side of the
			// fold (formula = n - (n-fold.value) * 2).
			if fold.direction == 'x' && p.x >= fold.value {
				acc.insert(Point {
					x: p.x - (p.x - fold.value) * 2,
					y: p.y,
				});
			} else if fold.direction == 'y' && p.y >= fold.value {
				acc.insert(Point {
					x: p.x,
					y: p.y - (p.y - fold.value) * 2,
				});
			} else {
				// If it's on the other side, then we just insert the original.
				acc.insert(*p);
			}
			acc
		});

		// Part 1 - print out the count of dots after the first
		// iteration.
		if first {
			first = false;
			println!("dots after first: {}", points.len());
		}
	}

	// Part 2 - we need to make lines.
	// Get the max values.
	let max_x = points.iter().map(|p| p.x).max().unwrap();
	let max_y = points.iter().map(|p| p.y).max().unwrap();
	for y in 0..=max_y {
		// Make a vector for this line with default empty values.
		let mut line: Vec<char> = vec![' '; max_x + 1_usize];
		// For each point on this line, mark it with X.
		points
			.iter()
			.filter(|p| p.y == y)
			.for_each(|p| line[p.x] = 'X');
		// Print it out.
		println!("{}", line.iter().collect::<String>());
	}
}
