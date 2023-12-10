use std::collections::HashMap;
use std::fs;

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let ff: Vec<isize> = lines
		.lines()
		.next()
		.unwrap()
		.split(',')
		.map(|s| s.parse::<isize>().unwrap())
		.collect();

	// Group up fish by age to reproduce.
	let mut ff: HashMap<isize, isize> = ff.iter().fold(HashMap::new(), |mut acc, i| {
		*acc.entry(*i).or_insert(0) += 1;
		acc
	});

	// We are just tracking the fish in each age, so each iteration,
	// we simply move them down and then put the zero ages in 6 and 8.
	for _ in 0..256 {
		let z = *ff.get(&0).unwrap_or(&0);
		for i in 0..8 {
			ff.insert(i, *ff.get(&(i + 1)).unwrap_or(&0));
		}
		*ff.entry(6).or_insert(0) += z; // add itself
		ff.insert(8, z); // add spawn
	}
	let sum = ff.iter().fold(0, |acc, (_, v)| acc + v);
	println!("{}", sum);
}
