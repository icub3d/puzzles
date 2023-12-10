use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

use lazy_static::lazy_static;

lazy_static! {
	// Scores for part 1.
	static ref SCORES: HashMap<char, usize> = {
		let mut scores = HashMap::new();
		scores.insert(')', 3);
		scores.insert(']', 57);
		scores.insert('}', 1197);
		scores.insert('>', 25137);
		scores
	};
	// Scores for part 2.
	static ref SCORES_2: HashMap<char, usize> = {
		let mut scores = HashMap::new();
		scores.insert('(', 1);
		scores.insert('[', 2);
		scores.insert('{', 3);
		scores.insert('<', 4);
		scores
	};

	// The association of pairs for lookup.
	static ref PAIRS: HashMap<char, char> = {
		let mut pairs = HashMap::new();
		pairs.insert(')', '(');
		pairs.insert(']', '[');
		pairs.insert('}', '{');
		pairs.insert('>', '<');
		pairs
	};

	// The left side of each pair.
	static ref LEFT: HashSet<char> = {
		let mut left = HashSet::new();
		left.insert('(');
		left.insert('[');
		left.insert('{');
		left.insert('<');
		left
	};
}

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let lines = lines.lines().collect::<Vec<&str>>();

	// Part 1 and 2 can be calculated in the same loop.
	let mut autocompletes = vec![];
	let illegal_score = lines.iter().fold(0, |acc, line| {
		// Use a stack to track the current left sides.
		let mut stack = vec![];
		for c in line.chars() {
			// If we have another left, just push it to the end.
			if LEFT.contains(&c) {
				stack.push(c);
				continue;
			}
			// Otherwise it's a right and we should make sure it has a
			// match on the stack.
			if !stack.is_empty() && stack.last().unwrap() != PAIRS.get(&c).unwrap() {
				// If it doesn't, we've found an illegal line and can
				// update our part 1 score and stop on this line.
				return acc + SCORES.get(&c).unwrap();
			}

			// We assume it's correct per description and pop it off.
			stack.pop();
		}

		// For part 2, let's see what's left in the stack and then give them a score
		autocompletes.push(
			stack
				.iter()
				.rev()
				.fold(0, |acc, c| acc * 5 + SCORES_2.get(c).unwrap()),
		);
		acc
	});
	println!("illegal score: {}", illegal_score);

	// Get the middle autocomplete score.
	autocompletes.sort_unstable();
	println!(
		"autocomplete score: {}",
		autocompletes[autocompletes.len() / 2]
	);
}
