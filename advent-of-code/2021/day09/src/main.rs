use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

const MAX: isize = 99;

macro_rules! point {
	($x:expr, $y:expr) => {
		&Point { x: $x, y: $y }
	};
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Point {
	x: isize,
	y: isize,
}

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let nn: HashMap<Point, isize> =
		lines
			.lines()
			.enumerate()
			.fold(HashMap::new(), |mut acc, (y, l)| {
				l.chars()
					.map(|c| c.to_digit(10).unwrap() as isize)
					.enumerate()
					.for_each(|(x, c)| {
						acc.insert(
							Point {
								x: x as isize,
								y: y as isize,
							},
							c,
						);
					});
				acc
			});

	// Find all the local minimums.
	let mm = nn
		.iter()
		.filter(|(k, v)| {
			// We check to make sure this is the lowest point around
			// itself.
			!(k.x > 0 && **v >= nn[point!(k.x - 1, k.y)]
				|| k.x < MAX && **v >= nn[point!(k.x + 1, k.y)]
				|| k.y > 0 && **v >= nn[point!(k.x, k.y - 1)]
				|| k.y < MAX && **v >= nn[point!(k.x, k.y + 1)])
		})
		.map(|(k, _)| k)
		.collect::<Vec<&Point>>();

	// Part 1 - sum up the local mins (+1).
	let sum = mm.iter().map(|m| nn[m] + 1).sum::<isize>();
	println!("sum of mins: {:?}", sum);

	// Part 2 - this is basically a "paint fill" function which can be
	// solved recursively.
	let mut sizes = mm
		.iter()
		.map(|m| fill(&nn, **m, &mut HashSet::new()))
		.collect::<Vec<isize>>();
	sizes.sort_unstable();
	let biggest = sizes
		.iter()
		.skip(sizes.len() - 3)
		.copied()
		.collect::<Vec<isize>>();
	println!(
		"biggest basins multiplied: {:?}",
		biggest.iter().product::<isize>()
	);
}

fn fill(nn: &HashMap<Point, isize>, m: Point, seen: &mut HashSet<Point>) -> isize {
	// If we've already been here or are of bounds or have reached the
	// edge of the basin, we are done.
	if seen.contains(&m) || m.x < 0 || m.x > MAX || m.y < 0 || m.y > MAX || nn[&m] == 9 {
		return 0;
	}
	// Add to seen list
	seen.insert(m);
	// sum up myself plus all those around me.
	1 + fill(nn, *point!(m.x - 1, m.y), seen)
		+ fill(nn, *point!(m.x + 1, m.y), seen)
		+ fill(nn, *point!(m.x, m.y - 1), seen)
		+ fill(nn, *point!(m.x, m.y + 1), seen)
}
