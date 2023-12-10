use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
struct Hand {
    cards: Vec<u8>,
    bid: usize,
    jokers: bool,
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.hand_type().cmp(&other.hand_type()) {
            std::cmp::Ordering::Equal => self.cards.cmp(&other.cards),
            other => other,
        }
    }
}

impl Hand {
    fn new(cards: Vec<u8>, bid: usize, jokers: bool) -> Self {
        Self { cards, bid, jokers }
    }

    fn char_to_value(c: char, jokers: bool) -> u8 {
        match c {
            'T' => 10,
            'J' => match jokers {
                true => 1,
                false => 11,
            },
            'Q' => 12,
            'K' => 13,
            'A' => 14,
            _ => c as u8 - b'0',
        }
    }

    fn parse(input: &str, jokers: bool) -> Self {
        let parts = input.split_whitespace().collect::<Vec<_>>();
        let cards = parts[0]
            .chars()
            .map(|c| Hand::char_to_value(c, jokers))
            .collect::<Vec<_>>();
        let bid = parts[1].parse::<usize>().unwrap();
        Hand::new(cards, bid, jokers)
    }

    fn hand_type(&self) -> u8 {
        let mut map = HashMap::new();
        let mut largest = 0;
        let mut largest_value = 0;
        if self.jokers {
            let joker_count = self.cards.iter().filter(|c| **c == 1).count();
            self.cards
                .iter()
                .filter(|c| **c != 1)
                .for_each(|c| *map.entry(c).or_insert(0) += 1);
            for (k, v) in map.iter() {
                if *v > largest_value {
                    largest = **k;
                    largest_value = *v;
                }
            }
            *map.entry(&largest).or_insert(0) += joker_count;
        } else {
            self.cards
                .iter()
                .for_each(|c| *map.entry(c).or_insert(0) += 1);
        }
        let counts = map.values().max().unwrap();
        match map.len() {
            1 => 7,
            2 => match counts {
                4 => 6,
                _ => 5,
            },
            3 => match counts {
                3 => 4,
                _ => 3,
            },
            4 => 2,
            _ => 1,
        }
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let mut hands = input
        .lines()
        .map(|l| Hand::parse(l, false))
        .collect::<Vec<_>>();
    hands.sort();
    let p1 = hands
        .iter()
        .enumerate()
        .map(|(i, h)| (i + 1) * h.bid)
        .sum::<usize>();
    println!("p1: {}", p1);

    let mut hands = input
        .lines()
        .map(|l| Hand::parse(l, true))
        .collect::<Vec<_>>();
    hands.sort();
    let p2 = hands
        .iter()
        .enumerate()
        .map(|(i, h)| (i + 1) * h.bid)
        .sum::<usize>();
    println!("p2: {}", p2);
}
