use std::collections::HashMap;
use std::collections::HashSet;

pub trait ToUsize {
	fn to_usize(&self) -> usize;
}

impl ToUsize for Vec<char> {
	fn to_usize(&self) -> usize {
		let mut power = 1;
		self.iter().rev().fold(0, |mut acc, c| {
			let i = match c {
				'#' => 1,
				_ => 0,
			};
			acc += i * power;
			power *= 2;
			acc
		})
	}
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct Point {
	x: isize,
	y: isize,
}

impl Point {
	fn new(x: isize, y: isize) -> Self {
		Self { x, y }
	}

	fn window(&self) -> Vec<Point> {
		vec![
			Point::new(self.x - 1, self.y + 1),
			Point::new(self.x, self.y + 1),
			Point::new(self.x + 1, self.y + 1),
			Point::new(self.x - 1, self.y),
			Point::new(self.x, self.y),
			Point::new(self.x + 1, self.y),
			Point::new(self.x - 1, self.y - 1),
			Point::new(self.x, self.y - 1),
			Point::new(self.x + 1, self.y - 1),
		]
	}

	fn window_of_windows(&self) -> HashMap<Point, Vec<Point>> {
		self.window().iter().map(|p| (*p, p.window())).collect()
	}
}

fn apply(image: &HashMap<Point, char>, algo: &[char], void: &char) -> HashMap<Point, char> {
	let mut handled = HashSet::new();
	// Our new image after changes.
	let mut new: HashMap<Point, char> = HashMap::new();

	// Go through each of our items
	for cur in image.keys() {
		// Generate all the surrounding windows.
		for (p, window) in cur.window_of_windows() {
			if handled.contains(&p) {
				continue;
			}
			// Turn the window into it's characters.
			let binary = window
				.iter()
				.map(|p| *image.get(p).unwrap_or(void))
				.collect::<Vec<char>>();

			// Get the value and put it into the image.
			new.insert(p, algo[binary.to_usize()]);
			handled.insert(p);
		}
	}
	new
}

#[allow(dead_code)]
fn print_image(image: &HashMap<Point, char>) {
	let mut top = 0;
	let mut bottom = 0;
	let mut right = 0;
	let mut left = 0;

	image.keys().for_each(|p| {
		if p.x < left {
			left = p.x;
		}
		if p.x > right {
			right = p.x;
		}
		if p.y < bottom {
			bottom = p.y;
		}
		if p.y > top {
			top = p.y;
		}
	});
	let mut y = top;
	while y >= bottom {
		for x in left..=right {
			print!("{}", image.get(&Point::new(x, y)).unwrap_or(&'.'));
		}
		println!();
		y -= 1;
	}
	println!();
}

fn main() {
	// Gather input.
	let input = include_str!("../input")
		.split("\n\n")
		.collect::<Vec<&str>>();
	let algo = input[0].chars().collect::<Vec<char>>();
	let mut image = input[1]
		.lines()
		.enumerate()
		.flat_map(|(y, l)| {
			l.chars()
				.enumerate()
				.map(|(x, c)| (Point::new(x as isize, -(y as isize)), c))
				.collect::<Vec<(Point, char)>>()
		})
		.collect::<HashMap<Point, char>>();

	// The key I was missing was that 0 and 512 were flipping, so the
	// expanded universe was flipping. This we need to flip what the
	// voids are.
	let mut void = algo.len() - 1;
	for i in 0..50 {
		image = apply(&image, &algo, &algo[void]);
		void = match void {
			0 => algo.len() - 1,
			_ => 0,
		};
		if i == 1 {
			println!("p1: {}", image.values().filter(|c| **c == '#').count());
		}
	}

	println!("p2: {}", image.values().filter(|c| **c == '#').count());
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_to_usize() {
		assert_eq!("..#...#.".chars().collect::<Vec<char>>().to_usize(), 34);
	}
}
