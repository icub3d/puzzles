use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;
use std::hash::Hash;

#[derive(Debug, Eq, Clone, Copy, PartialEq, Hash)]
struct Point {
	x: isize,
	y: isize,
}

impl Point {
	pub fn parse(s: &str) -> Self {
		let parts: Vec<&str> = s.split(',').collect();
		Self {
			x: parts[0].parse::<isize>().unwrap(),
			y: parts[1].parse::<isize>().unwrap(),
		}
	}
}

#[derive(Debug, Eq, Clone, Copy, PartialEq, Hash)]
struct Pair {
	a: Point,
	b: Point,
}

impl Pair {
	pub fn parse(s: &str) -> Self {
		let parts: Vec<&str> = s.split(" -> ").collect();
		Self {
			a: Point::parse(parts[0]),
			b: Point::parse(parts[1]),
		}
	}
	pub fn line(&self) -> Vec<Point> {
		let delta_x: isize = match self.a.x.cmp(&self.b.x) {
			Ordering::Less => 1,
			Ordering::Greater => -1,
			Ordering::Equal => 0,
		};
		let delta_y: isize = match self.a.y.cmp(&self.b.y) {
			Ordering::Less => 1,
			Ordering::Greater => -1,
			Ordering::Equal => 0,
		};
		let mut cur = self.a;
		let mut v: Vec<Point> = vec![cur];
		while cur != self.b {
			cur = Point {
				x: cur.x + delta_x,
				y: cur.y + delta_y,
			};
			v.push(cur);
		}
		v
	}
}

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let pairs: Vec<Pair> = lines.lines().map(Pair::parse).collect();

	let hv: Vec<Point> = pairs.iter().map(|p| p.line()).flatten().collect();
	let hv: HashMap<Point, usize> = hv.iter().fold(HashMap::new(), |mut acc, pair| {
		*acc.entry(*pair).or_insert(0) += 1;
		acc
	});

	let hv: usize = hv.iter().fold(0, |acc, (_, v)| match *v > 1 {
		true => acc + 1,
		false => acc,
	});

	println!("{}", hv);
}
