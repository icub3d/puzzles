use std::{cmp::max, str::FromStr};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Pair {
    start: usize,
    end: usize,
}

impl FromStr for Pair {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split('-');
        let start = iter.next().unwrap().parse::<usize>().unwrap();
        let end = iter.next().unwrap().parse::<usize>().unwrap();
        Ok(Pair { start, end })
    }
}

impl Pair {
    fn new(start: usize, end: usize) -> Self {
        Pair { start, end }
    }

    fn overlaps(&self, other: &Pair) -> bool {
        other.start >= self.start && other.start <= self.end
            || other.end >= self.start && other.end <= self.end
    }

    fn merge(&self, other: &Pair) -> Pair {
        Pair::new(self.start.min(other.start), self.end.max(other.end))
    }
}

fn main() {
    let input = include_str!("../input");
    let mut pairs = input
        .lines()
        .map(|line| line.parse::<Pair>().unwrap())
        .collect::<Vec<_>>();
    pairs.sort();

    let mut min = 0;
    for Pair { start, end } in &pairs {
        if start > &min {
            break;
        }
        min = max(min, end + 1);
    }
    println!("p1: {}", min);

    let mut merged = vec![pairs[0]];
    for pair in pairs.iter().skip(1) {
        let last = merged[merged.len() - 1];
        if last.overlaps(pair) {
            merged.pop();
            merged.push(last.merge(pair));
        } else {
            merged.push(*pair);
        }
    }
    let blocked = merged
        .iter()
        .map(|pair| pair.end - pair.start + 1)
        .sum::<usize>();
    println!("p2: {}", 4294967295 - blocked + 1);
}
