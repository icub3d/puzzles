#![feature(test)]
extern crate test;

static INPUT: &str = include_str!("../input");

fn main() {
    println!("part1:        {}", part1(INPUT));
    println!("part1_faster: {}", part1_faster(INPUT));
    println!("part2:        {}", part2(INPUT));
    println!("part2_faster: {}", part2_faster(INPUT));
}

static WORDS: [&[u8]; 9] = [
    b"one", b"two", b"three", b"four", b"five", b"six", b"seven", b"eight", b"nine",
];

pub fn part2_faster(input: &str) -> usize {
    input
        .lines()
        .map(|line| {
            let mut line = line.as_bytes();
            let first = 'first: loop {
                if line[0].is_ascii_digit() {
                    break 'first line[0].wrapping_sub(b'0') as usize;
                }
                for (i, word) in WORDS.iter().enumerate() {
                    if line.starts_with(word) {
                        break 'first i + 1;
                    }
                }

                line = &line[1..];
            };

            let last = 'last: loop {
                if line[line.len() - 1].is_ascii_digit() {
                    break 'last line[line.len() - 1].wrapping_sub(b'0') as usize;
                }
                for (i, word) in WORDS.iter().enumerate() {
                    if line.ends_with(word) {
                        break 'last i + 1;
                    }
                }

                line = &line[..line.len() - 1];
            };

            first * 10 + last
        })
        .sum()
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

pub fn find_first_digit(chars: &[char], traversal: &[usize]) -> Option<char> {
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

pub fn part1(input: &str) -> u32 {
    let mut total = 0;

    for line in input.lines() {
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

pub fn part1_faster(input: &str) -> u32 {
    input
        .lines()
        .map(|line| {
            let first = line
                .bytes()
                .find(u8::is_ascii_digit)
                .unwrap()
                .wrapping_sub(b'0');
            let last = line
                .bytes()
                .rfind(u8::is_ascii_digit)
                .unwrap()
                .wrapping_sub(b'0');
            (first * 10 + last) as u32
        })
        .sum()
}

pub fn part2(input: &str) -> u32 {
    let mut total = 0;

    for line in input.lines() {
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

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[bench]
    fn bench_part1(b: &mut Bencher) {
        b.iter(|| part1(INPUT));
    }

    #[bench]
    fn bench_part1_faster(b: &mut Bencher) {
        b.iter(|| part1_faster(INPUT));
    }

    #[bench]
    fn bench_part2(b: &mut Bencher) {
        b.iter(|| part2(INPUT));
    }

    #[bench]
    fn bench_part2_faster(b: &mut Bencher) {
        b.iter(|| part2_faster(INPUT));
    }
}
