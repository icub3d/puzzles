use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

#[derive(Hash, Debug, Eq, Copy, Clone, PartialEq)]
struct Point {
	x: isize,
	y: isize,
}

impl Point {
	fn new(x: isize, y: isize) -> Self {
		Point { x, y }
	}

	fn adjacent(&self) -> Vec<Point> {
		(-1..=1)
			.map(|x| (-1..=1).map(|y| Point { x, y }).collect::<Vec<Point>>())
			.flatten()
			.map(|p| Point {
				x: self.x + p.x,
				y: self.y + p.y,
			})
			.filter(|p| p.x >= 0 && p.x <= 9 && p.y >= 0 && p.y <= 9 && p != self)
			.collect::<Vec<Point>>()
	}
}

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let mut grid: HashMap<Point, isize> =
		lines
			.lines()
			.enumerate()
			.fold(HashMap::new(), |mut acc, (y, l)| {
				l.chars().enumerate().for_each(|(x, c)| {
					acc.insert(
						Point::new(x as isize, y as isize),
						c.to_digit(10).unwrap() as isize,
					);
				});
				acc
			});

	let mut flashes = 0;
	let mut counter = 0;
	loop {
		// Increase them all by one.
		for (_, v) in grid.iter_mut() {
			*v += 1;
		}

		// Maintain a set of all that need to flash.
		let mut flashing = grid
			.iter()
			.filter(|(_, v)| **v > 9)
			.map(|(k, _)| k)
			.cloned()
			.collect::<HashSet<Point>>();

		// The set of those that have flashed so we don't
		// flash them again.
		let mut flashed: HashSet<Point> = HashSet::new();
		// Handle all the flashing
		while !flashing.is_empty() {
			let cur = flashing.iter().next().cloned().unwrap();
			flashing.remove(&cur);
			flashed.insert(cur);
			// Find all the adjacent and increase them.
			for a in cur.adjacent() {
				*grid.entry(a).or_insert(0) += 1;
				if grid[&a] > 9 && !flashed.contains(&a) {
					flashing.insert(a);
				}
			}
		}
		flashes += flashed.len();
		// Before we continue, we need to reset any that are larger.
		for (_, v) in grid.iter_mut() {
			if *v > 9 {
				*v = 0;
			}
		}

		// Part 1 report at 100.
		counter += 1;
		if counter == 100 {
			println!("flashes: {}", flashes);
		}

		if flashed.len() == 100 {
			println!("all flashed: {}", counter);
			break;
		}
	}
}
