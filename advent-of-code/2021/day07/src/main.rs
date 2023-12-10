use std::fs;

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let mut ff: Vec<isize> = lines
		.lines()
		.next()
		.unwrap()
		.split(',')
		.map(|s| s.parse::<isize>().unwrap())
		.collect();
	ff.sort_unstable();

	// Both sides happen to be the same number, so we are OK here.
	let median = &ff[ff.len() / 2];

	println!("median: {:?}", median);
	let cost = ff.iter().fold(0, |acc, x| acc + (x - median).abs());
	println!("cost: {:?}", cost);

	let mean: f64 = ff.iter().sum::<isize>() as f64 / ff.len() as f64;
	let mean: isize = mean.floor() as isize;
	println!("mean: {}", mean);
	let cost: isize = ff
		.iter()
		.map(|x| (x - mean).abs())
		.map(|x| (x * (x + 1)) / 2)
		.sum::<isize>();
	println!("cost: {}", cost);
}
