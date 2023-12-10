#[derive(Clone, Eq, PartialEq, Debug)]
enum Token {
	LeftBracket,
	RightBracket,
	Comma,
	Value(usize),
}

// Helper function for printing.
// fn to_string(tokens: &[Token]) -> String {
// 	let mut vv = vec![];
// 	for t in tokens.iter() {
// 		match *t {
// 			Token::LeftBracket => vv.push('['),
// 			Token::RightBracket => vv.push(']'),
// 			Token::Comma => vv.push(','),
// 			Token::Value(n) => format!("{}", n).chars().for_each(|c| vv.push(c)),
// 		}
// 	}
// 	vv.iter().collect()
// }

fn add(left: &[Token], right: &[Token]) -> Vec<Token> {
	// Create a new vector from the two surrounded by brackets with a
	// comma in between.
	let mut sum = vec![Token::LeftBracket];
	sum.extend(left.iter().cloned());
	sum.push(Token::Comma);
	sum.extend(right.iter().cloned());
	sum.push(Token::RightBracket);
	reduce(&sum)
}

fn explode(tokens: &mut Vec<Token>) -> bool {
	// Find the first thing to explode.
	let mut depth = 0;
	let pos = tokens.iter().position(|t| {
		match t {
			Token::LeftBracket => depth += 1,
			Token::RightBracket => depth -= 1,
			_ => (),
		}
		depth == 5
	});

	if let Some(pos) = pos {
		// Get our two numbers.
		let left = match tokens[pos + 1] {
			Token::Value(v) => v,
			_ => panic!("unexpected token at {}", pos),
		};
		let right = match tokens[pos + 3] {
			Token::Value(v) => v,
			_ => panic!("unexpected token at {}", pos),
		};

		// Find the previous and next values and increment them.
		if let Some(v) = tokens[..pos]
			.iter_mut()
			.rev()
			.filter_map(|t| {
				if let Token::Value(i) = t {
					Some(i)
				} else {
					None
				}
			})
			.next()
		{
			*v += left;
		}

		if let Some(v) = tokens[pos + 4..] // after end of this pair.
			.iter_mut()
			.filter_map(|t| {
				if let Token::Value(i) = t {
					Some(i)
				} else {
					None
				}
			})
			.next()
		{
			*v += right;
		}

		// Add our zero
		tokens[pos] = Token::Value(0);
		// Remove our values we don't need anymore.
		tokens.drain(pos + 1..pos + 5).for_each(drop);
		return true;
	}
	false
}

fn split(tokens: &mut Vec<Token>) -> bool {
	// Find first number that's too large.
	let pos = tokens
		.iter()
		.position(|t| matches!(t, Token::Value(v) if *v >= 10));
	if let Some(pos) = pos {
		let v = match tokens[pos] {
			Token::Value(v) => v,
			_ => panic!("unexpected token at position {}", pos),
		};

		let left = v / 2;
		let right = v - left;

		// We are removing our value, and replacing it with two
		// bracketed numbers. The current position will be come the
		// right bracket and then we just splice the new stuff in
		// front.
		tokens[pos] = Token::RightBracket;
		tokens.splice(
			pos..pos,
			[
				Token::LeftBracket,
				Token::Value(left),
				Token::Comma,
				Token::Value(right),
			],
		);
		return true;
	}
	false
}

fn reduce(tokens: &[Token]) -> Vec<Token> {
	let mut tokens = tokens.to_vec();
	loop {
		// Try to explode then try to split. If we don't do either, we
		// are done. Only do one of them though.
		if explode(&mut tokens) || split(&mut tokens) {
			continue;
		}
		break;
	}
	tokens
}

fn magnitude(tokens: &[Token]) -> usize {
	// We can just track the multiplier which increases or decreases
	// based on where we are. This works because the farther we go
	// down the pairs, the factor in which the number increases or
	// decreases is equal to it's depth.
	let mut multiplier = 1;
	tokens.iter().fold(0, |acc, t| match t {
		Token::LeftBracket => {
			// If we are going down the left bracket, we multiply by three.
			multiplier *= 3;
			acc
		}
		Token::RightBracket => {
			// We are going out, so we remove the right side multiplier.
			multiplier /= 2;
			acc
		}
		Token::Comma => {
			// We are now on the right side, so we have to divide by
			// three to get rid of the multiplier of the left side and
			// then multiply by two.
			multiplier /= 3;
			multiplier *= 2;
			acc
		}
		Token::Value(v) => acc + v * multiplier,
	})
}

// Helper functions to turn a string into a tokenized number.
fn number(s: &str) -> Vec<Token> {
	s.chars()
		.map(|c| match c {
			'[' => Token::LeftBracket,
			']' => Token::RightBracket,
			',' => Token::Comma,
			_ => Token::Value(c.to_digit(10).unwrap() as usize),
		})
		.collect::<Vec<Token>>()
}

// Add all of the given numbers.
fn add_all(numbers: &[Vec<Token>]) -> Vec<Token> {
	numbers
		.iter()
		.skip(1)
		.fold(numbers[0].clone(), |acc, n| add(&acc, n))
}

fn main() {
	let input = include_str!("../input");
	// NOTE: I started with a tree style thing but got annoyed at the
	// limitations of doing it safely in rust. It occurred to me that
	// I might be able to more easily do part one simply by using
	// tokens because of how the explode and split work. Hopefully
	// part 2 doesn't make this harder.
	let nn = input.lines().map(number).collect::<Vec<Vec<Token>>>();
	let n = add_all(&nn);
	println!("p1: {}", magnitude(&n));

	// We just try adding all of the numbers and keep the highest.
	let mut highest = 0;
	for x in 0..nn.len() {
		for y in 0..nn.len() {
			if x == y {
				continue;
			}

			highest = highest.max(magnitude(&add(&nn[x], &nn[y])));
		}
	}
	println!("p2: {}", highest);
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_simple() {
		let s = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n";
		let nn = s.lines().map(|l| number(&l)).collect::<Vec<Vec<Token>>>();
		let exp = "[[[[1,1],[2,2]],[3,3]],[4,4]]";
		let exp = number(&exp);
		assert_eq!(add_all(&nn), exp);
	}
	#[test]
	fn test_simple_explode() {
		let s = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n";
		let nn = s.lines().map(|l| number(&l)).collect::<Vec<Vec<Token>>>();
		let exp = "[[[[3,0],[5,3]],[4,4]],[5,5]]";
		let exp = number(&exp);
		assert_eq!(add_all(&nn), exp);
	}
	#[test]
	fn test_simple_explode_and_split() {
		let s = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]";
		let nn = s.lines().map(|l| number(&l)).collect::<Vec<Vec<Token>>>();
		let exp = "[[[[5,0],[7,4]],[5,5]],[6,6]]";
		let exp = number(&exp);
		assert_eq!(add_all(&nn), exp);
	}
	#[test]
	fn test_larger() {
		let s = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]";
		let nn = s.lines().map(|l| number(&l)).collect::<Vec<Vec<Token>>>();
		let exp = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]";
		let exp = number(&exp);
		assert_eq!(add_all(&nn), exp);
	}
}
