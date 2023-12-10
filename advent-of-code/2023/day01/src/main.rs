fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let lines = input.lines().collect::<Vec<_>>();

    println!("p1: {}", part1(&lines));
    println!("p2: {}", part2(&lines));
}

fn part1(lines: &[&str]) -> u32 {
    let mut total = 0;

    for line in lines {
        let digits = line
            .chars()
            .filter(|c| c.is_ascii_digit())
            .map(|c| c.to_digit(10).unwrap())
            .collect::<Vec<_>>();
        let number = digits[0] * 10 + digits[digits.len() - 1];
        total += number;
    }

    total
}

// Check for "one", "two", etc. as a prefix and return it's
// requivalent character if found.
fn has_prefixes(line: &str) -> (char, bool) {
    let prefixes = vec![
        ("one", '1'),
        ("two", '2'),
        ("three", '3'),
        ("four", '4'),
        ("five", '5'),
        ("six", '6'),
        ("seven", '7'),
        ("eight", '8'),
        ("nine", '9'),
    ];
    for (prefix, character) in prefixes {
        if line.starts_with(prefix) {
            return (character, true);
        }
    }
    (' ', false)
}

fn find_first_digit(chars: &[char], traversal: &[usize]) -> Option<char> {
    for i in traversal {
        if chars[*i].is_ascii_digit() {
            return Some(chars[*i]);
        }
        let (c, found) = has_prefixes(&chars[*i..].iter().collect::<String>());
        if found {
            return Some(c);
        }
    }
    None
}

fn part2(lines: &[&str]) -> u32 {
    let mut total = 0;

    for line in lines {
        // find the first and last digits in the string.
        let chars = line.chars().collect::<Vec<_>>();
        let first = find_first_digit(&chars, &(0..chars.len()).collect::<Vec<_>>()).unwrap();
        let last = find_first_digit(&chars, &(0..chars.len()).rev().collect::<Vec<_>>()).unwrap();

        let first = first.to_digit(10).unwrap();
        let last = last.to_digit(10).unwrap();
        let number = first * 10 + last;
        total += number;
    }

    total
}
