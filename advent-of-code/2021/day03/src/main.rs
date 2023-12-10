use std::fs;

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let lines = lines.lines().collect::<Vec<&str>>();

	// power consumption
	let g = gamma(&lines);
	let e = epsilon(&g);
	println!("power consumption: {}", to_int(&g) * to_int(&e));

	// o2 rating
	let mut o2 = lines.clone();
	let mut n = 0;
	while o2.len() > 1 {
		let g = gamma(&o2);
		let c = g.chars().nth(n).unwrap();
		o2 = o2
			.iter()
			.filter(|&x| x.chars().nth(n).unwrap() == c)
			.copied()
			.collect();
		n += 1;
	}
	let o2 = o2[0];

	// o2 rating
	let mut co2 = lines.clone();
	let mut n = 0;
	while co2.len() > 1 {
		let e = epsilon(&gamma(&co2));
		let c = e.chars().nth(n).unwrap();
		co2 = co2
			.iter()
			.filter(|&x| x.chars().nth(n).unwrap() == c)
			.copied()
			.collect();
		n += 1;
	}
	let co2 = co2[0];
	println!("life support rating: {:?}", to_int(o2) * to_int(co2));
}

fn to_int(s: &str) -> isize {
	isize::from_str_radix(s, 2).unwrap()
}
fn gamma(lines: &[&str]) -> String {
	let init = vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
	let gamma = lines.iter().fold(init, |mut acc, line| {
		for (i, c) in line.chars().enumerate() {
			acc[i] += match c {
				'0' => -1,
				'1' => 1,
				_ => 0,
			};
		}
		acc
	});
	gamma
		.iter()
		.map(|i| match *i < 0 {
			true => '0',
			false => '1',
		})
		.collect()
}

fn epsilon(gamma: &str) -> String {
	gamma
		.chars()
		.map(|c| match c {
			'0' => '1',
			'1' => '0',
			_ => '0',
		})
		.collect()
}
