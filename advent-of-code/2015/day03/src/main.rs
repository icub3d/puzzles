use std::collections::HashSet;

fn main() {
	let input = std::fs::read_to_string("input").unwrap();
	let (mut x, mut y) = (0, 0);
	let mut visited: HashSet<(isize, isize)> = HashSet::new();
	visited.insert((0,0));
	for c in input.chars() {
		match c {
			'>' => x += 1,
			'<' => x -= 1,
			'v' => y -= 1,
			'^' => y += 1,
			_ => ()
		};
		visited.insert((x, y));
	}
	println!("p1: {}", visited.len());

	let (mut x, mut y, mut i, mut j) = (0, 0, 0, 0);
	let mut visited: HashSet<(isize, isize)> = HashSet::new();
	visited.insert((0,0));
	let mut santa = true;
	for c in input.chars() {
		match santa {
			true => {
				match c {
					'>' => x += 1,
					'<' => x -= 1,
					'v' => y -= 1,
					'^' => y += 1,
					_ => ()
				};
				visited.insert((x, y));
			},

			false => {
				match c {
					'>' => i += 1,
					'<' => i -= 1,
					'v' => j -= 1,
					'^' => j += 1,
					_ => ()
				};
				visited.insert((i, j));
				
			},
		}
		santa = !santa;
	}
	println!("p2: {}", visited.len());
}
