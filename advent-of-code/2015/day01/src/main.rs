fn main() {
	let input = std::fs::read_to_string("input").unwrap();
	let floor: i64 = input.chars().map(|c| match c {
		'(' => 1,
		')' => -1,
		_ => 0, // new line at end.
	}).sum();
	println!("p1: {}", floor);

	let mut floor = 0;
	for (i, c) in input.chars().enumerate() {
		floor += match c {
			'(' => 1,
			')' => -1,
			_ => 0, // new line at end.
		};
		if floor == -1 {
			println!("p2: {}", i+1);
			break;
		}
	}
}
