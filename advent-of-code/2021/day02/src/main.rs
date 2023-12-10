use std::fs;

fn main() {
	let course = fs::read_to_string("input").unwrap();
	let course: Vec<(&str, usize)> = course
		.lines()
		.map(|line| {
			let parts: Vec<&str> = line.split(' ').collect();
			(parts[0], parts[1].parse::<usize>().unwrap())
		})
		.collect();

	let mut x = 0;
	let mut y = 0;
	for (direction, delta) in course.iter() {
		match *direction {
			"forward" => y += delta,
			"down" => x += delta,
			"up" => x -= delta,
			_ => {}
		}
	}
	println!("{}", x * y);

	let mut x = 0;
	let mut y = 0;
	let mut aim = 0;
	for (direction, delta) in course.iter() {
		match *direction {
			"forward" => {
				y += delta;
				x += aim * delta
			}
			"down" => aim += delta,
			"up" => aim -= delta,
			_ => {}
		}
	}
	println!("{}", x * y);
}
