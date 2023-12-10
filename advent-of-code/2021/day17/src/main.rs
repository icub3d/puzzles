use std::cmp::Ordering;

#[derive(Debug)]
struct Point {
	x: isize,
	y: isize,
}

impl Point {
	fn new(x: isize, y: isize) -> Self {
		Self { x, y }
	}
}

#[derive(Debug)]
struct Target {
	top_left: Point,
	bottom_right: Point,
}

impl Target {
	fn new(xs: &[isize], ys: &[isize]) -> Self {
		Self {
			top_left: Point::new(xs[0], ys[1]),
			bottom_right: Point::new(xs[1], ys[0]),
		}
	}

	fn contains(&self, p: &Point) -> bool {
		p.x >= self.top_left.x
			&& p.x <= self.bottom_right.x
			&& p.y <= self.top_left.y
			&& p.y >= self.bottom_right.y
	}

	fn overshot(&self, p: &Point) -> bool {
		p.x > self.bottom_right.x || p.y < self.bottom_right.y
	}
}

#[derive(Debug)]
struct Probe {
	position: Point,
	delta: Point,
}

impl Probe {
	fn new(dx: isize, dy: isize) -> Self {
		Self {
			position: Point::new(0, 0),
			delta: Point::new(dx, dy),
		}
	}

	fn step(&mut self) {
		self.position.x += self.delta.x;
		self.position.y += self.delta.y;
		self.delta.x = match self.delta.x.cmp(&0) {
			Ordering::Less => self.delta.x + 1,
			Ordering::Greater => self.delta.x - 1,
			Ordering::Equal => self.delta.x,
		};
		self.delta.y -= 1;
	}
}

fn main() {
	// More lines used to parse than could have done by just inserting
	// the values, LOL.
	let input = include_str!("../input").lines().next().unwrap();
	let input = input
		.trim_start_matches("target area: ")
		.split(", ")
		.collect::<Vec<&str>>();
	let xx = input[0]
		.trim_start_matches("x=")
		.split("..")
		.map(|s| s.parse::<isize>().unwrap())
		.collect::<Vec<isize>>();
	let yy = input[1]
		.trim_start_matches("y=")
		.split("..")
		.map(|s| s.parse::<isize>().unwrap())
		.collect::<Vec<isize>>();

	let target = Target::new(&xx, &yy);

	// P1 - just try a bunch and see which is highest.
	// P2 - count all the solutions we find.
	let mut highest = isize::MIN;
	let mut solutions = 0;
	for x in 1..1000 {
		for y in -1000..1000 {
			// Create a new probe with new initial velocity.
			let mut p = Probe::new(x, y);
			// Track the highest y this initial velocity sees.
			let mut local_highest = p.position.y;

			// Keep going until we've overshot the target.
			while !target.overshot(&p.position) {
				// Take a step and update our local highest.
				p.step();
				local_highest = local_highest.max(p.position.y);
				// If we hit the target, we've found a solution. See
				// if it's higher than our total highest.
				if target.contains(&p.position) {
					solutions += 1;
					highest = highest.max(local_highest);
					break;
				}
			}
		}
	}
	println!("p1 (loop): {}", highest);
	println!("p2 (solutions): {}", solutions);
}
