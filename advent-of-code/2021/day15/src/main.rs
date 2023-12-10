use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashMap;

use lazy_static::lazy_static;
lazy_static! {
	static ref DELTAS: Vec<Point> = {
		vec![
			Point::new(-1, 0),
			Point::new(1, 0),
			Point::new(0, -1),
			Point::new(0, 1),
		]
	};
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct Point {
	x: isize,
	y: isize,
}

impl Point {
	fn new(x: isize, y: isize) -> Self {
		Self { x, y }
	}

	fn edges(&self, end: &Point) -> Vec<Point> {
		DELTAS
			.iter()
			.map(|d| Point::new(self.x + d.x, self.y + d.y))
			.filter(|p| p.x >= 0 && p.x <= end.y && p.y >= 0 && p.y <= end.y)
			.collect()
	}
}

impl Ord for Point {
	fn cmp(&self, other: &Self) -> Ordering {
		other.x.cmp(&self.x).then_with(|| self.y.cmp(&other.y))
	}
}

impl PartialOrd for Point {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
	cost: isize,
	point: Point,
}

impl Ord for State {
	fn cmp(&self, other: &Self) -> Ordering {
		other
			.cost
			.cmp(&self.cost)
			.then_with(|| self.point.cmp(&other.point))
	}
}

impl PartialOrd for State {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

// We use Dijkstra's algorithm slightly modified using a priority
// queue to calculate shortest path.
fn dijkstra(weights: &HashMap<Point, isize>, start: Point, end: Point) -> Option<isize> {
	// Normally we'd set the value to isize::MAX but we can use a
	// default value if it's not in the map already (see get() methods
	// down below.)
	let mut dist: HashMap<Point, isize> = HashMap::new();
	let mut heap = BinaryHeap::new();

	// Setup our beginning position.
	dist.insert(start, 0);
	heap.push(State {
		cost: 0,
		point: start,
	});

	// We keep popping off the heap until it's empty.
	while let Some(State { cost, point }) = heap.pop() {
		// If we hit the end, we know the shortest distance now.
		if point == end {
			return Some(cost);
		}

		// If we have a distance already and it's smaller, we don't
		// need to keep down this path.
		if cost > *dist.get(&point).unwrap_or(&isize::MAX) {
			continue;
		}

		// Loop through each of this points edges.
		for edge in point.edges(&end) {
			// Our new state will be this cost plus the cost of moving
			// to the edge. We'll only push it on the heap if this
			// cost is lower. We need to calculate the cost based on
			// the weights given but they may be a different value if
			// they are on a different tile in the map.
			let dx = edge.x / 100;
			let dy = edge.y / 100;
			let p = Point::new(edge.x % 100, edge.y % 100);
			let mut weight = weights[&p] + dx + dy;
			if weight > 9 {
				weight %= 9;
			}
			let next = State {
				cost: cost + weight,
				point: edge,
			};
			if next.cost < *dist.get(&next.point).unwrap_or(&isize::MAX) {
				heap.push(next);
				dist.insert(next.point, next.cost);
			}
		}
	}
	None
}

fn main() {
	let lines = include_str!("../input").lines();

	// Create our weights for each position in the original input.
	let weights = lines
		.enumerate()
		.flat_map(|(y, l)| {
			l.chars()
				.map(|c| c.to_digit(10).unwrap())
				.enumerate()
				.map(move |(x, i)| (Point::new(x as isize, y as isize), i as isize))
		})
		.collect::<HashMap<Point, isize>>();

	// Part 1
	println!(
		"p1: {:?}",
		dijkstra(&weights, Point::new(0, 0), Point::new(99, 99)).unwrap()
	);

	// Part 2
	println!(
		"p2: {:?}",
		dijkstra(&weights, Point::new(0, 0), Point::new(499, 499)).unwrap()
	);
}
