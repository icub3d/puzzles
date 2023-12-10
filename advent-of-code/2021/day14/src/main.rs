use std::collections::HashMap;

type Pair = (char, char);

fn run(polymer: &[char], rules: &HashMap<Pair, char>, steps: usize) -> usize {
	// Create a set of pairs and their count. We really just need to
	// track how many we have of each pair in the current chain.
	let mut pairs: HashMap<Pair, usize> =
		polymer
			.windows(2)
			.map(|p| (p[0], p[1]))
			.fold(HashMap::new(), |mut acc, p| {
				acc.entry(p).and_modify(|v| *v += 1).or_insert(1);
				acc
			});

	// We can also track the count of each letter as we are
	// iterating. We start with our current list of letters and then
	// any time we add a letter, we increment the count.
	let mut letters: HashMap<char, usize> = polymer.iter().fold(HashMap::new(), |mut acc, c| {
		acc.entry(*c).and_modify(|v| *v += 1).or_insert(1);
		acc
	});

	for _ in 0..steps {
		// Create a new list. Curious if we could do this with the
		// same map.
		let mut updated: HashMap<Pair, usize> = HashMap::new();
		for (k, v) in pairs.clone().iter() {
			// Update the count for each new pair created. You'll have
			// (left, new) and (new, right).
			let new = rules[k];
			updated
				.entry((k.0, new))
				.and_modify(|a| *a += v)
				.or_insert(*v);
			updated
				.entry((new, k.1))
				.and_modify(|a| *a += v)
				.or_insert(*v);

			// Update our counts.
			letters.entry(new).and_modify(|a| *a += v).or_insert(*v);
		}
		pairs = updated;
	}

	letters.values().max().unwrap() - letters.values().min().unwrap()
}

fn main() {
	let input = include_str!("../input")
		.split("\n\n")
		.collect::<Vec<&str>>();
	let polymer = input[0]
		.lines()
		.next()
		.unwrap()
		.chars()
		.collect::<Vec<char>>();
	let rules = input[1]
		.lines()
		.map(|l| {
			let parts = l.split(" -> ").collect::<Vec<&str>>();
			let mut iter = parts[0].chars();
			(
				(iter.next().unwrap(), iter.next().unwrap()),
				parts[1].chars().next().unwrap(),
			)
		})
		.collect::<HashMap<Pair, char>>();

	println!("p1: {:?}", run(&polymer, &rules, 10));
	println!("p2: {:?}", run(&polymer, &rules, 40));
}
