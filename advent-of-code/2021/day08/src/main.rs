use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
// 0 - 6
// 1 - 2
// 2 - 5
// 3 - 5
// 4 - 4
// 5 - 5
// 6 - 6
// 7 - 3
// 8 - 7
// 9 - 6

#[derive(Debug)]
struct Display {
	// Did some analysis and each line contains each of the digits
	// just in different orders.
	patterns: Vec<HashSet<char>>,
	output: Vec<HashSet<char>>,
}

macro_rules! find {
	($on:expr, $p:expr) => {
		Some($on.iter().filter($p).next().unwrap())
	};
}

impl Display {
	fn new(line: &str) -> Self {
		let parts: Vec<&str> = line.split(" | ").collect();
		let patterns = parts[0]
			.split(' ')
			.map(|s| s.chars().collect::<HashSet<char>>())
			.collect();
		let output = parts[1]
			.split(' ')
			.map(|s| s.chars().collect::<HashSet<char>>())
			.collect();
		Self { patterns, output }
	}

	fn solution(&self) -> usize {
		let mappings = self.mappings();
		let mut sum = 0;
		let mul = vec![1000, 100, 10, 1]; // multiplier for positions
		for (i, v) in self.output.iter().enumerate() {
			let mut v = v.iter().cloned().collect::<Vec<char>>();
			v.sort_unstable();
			let v = v.iter().collect::<String>();
			sum += mappings[&v] * mul[i]
		}
		sum
	}
	fn mappings(&self) -> HashMap<String, usize> {
		let mut vv: Vec<Option<&HashSet<char>>> = vec![None; 10];
		// 1, 4, 7, 8 are easy matches.
		vv[1] = find!(self.patterns, |p| p.len() == 2);
		vv[4] = find!(self.patterns, |p| p.len() == 4);
		vv[7] = find!(self.patterns, |p| p.len() == 3);
		vv[8] = find!(self.patterns, |p| p.len() == 7);

		// The rest can be created by comparing them to the known ones.
		// 6 should only have one value from 1.
		vv[6] = find!(self.patterns, |p| p.len() == 6
			&& p.difference(vv[1].unwrap()).count() == 5);

		// 9 should have 2 values from 4.
		vv[9] = find!(self.patterns, |p| p.len() == 6
			&& p.difference(vv[4].unwrap()).count() == 2);

		// 0 is the only one with size 6 left.
		vv[0] = find!(self.patterns, |p| p.len() == 6
			&& *p != vv[9].unwrap()
			&& *p != vv[6].unwrap());

		// 3 contains two from 1.
		vv[3] = find!(self.patterns, |p| p.len() == 5
			&& p.difference(vv[1].unwrap()).count() == 3);

		// 2 contains all but one of 9.
		vv[2] = find!(self.patterns, |p| p.len() == 5
			&& p.difference(vv[9].unwrap()).count() == 1);

		// 5 is the only one with length 5 left.
		vv[5] = find!(self.patterns, |p| p.len() == 5
			&& *p != vv[3].unwrap()
			&& *p != vv[2].unwrap());

		// Convert to a map for lookup.
		let mut mappings = HashMap::new();
		for (i, v) in vv.iter().enumerate() {
			let mut v = v.unwrap().iter().cloned().collect::<Vec<char>>();
			v.sort_unstable();
			mappings.insert(v.iter().collect(), i);
		}
		mappings
	}
}

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let displays: Vec<Display> = lines.lines().map(Display::new).collect();

	// part 1
	let sum = displays
		.iter()
		.map(|d| d.output.clone())
		.flatten()
		.filter(|d| matches!(d.len(), 2 | 4 | 3 | 7))
		.count();
	println!("1,4,7,8: {}", sum);

	// Part 2
	let sum = displays.iter().map(|d| d.solution()).sum::<usize>();
	println!("sum: {}", sum);
}
