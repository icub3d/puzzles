use std::{collections::HashSet, fs};

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let lines = lines.lines().collect::<Vec<&str>>();
    let mut p1 = 0;

    for line in lines.iter() {
        let cc = line.chars().collect::<Vec<char>>();
        let left = cc[..cc.len() / 2]
            .iter()
            .cloned()
            .collect::<HashSet<char>>();
        let right = cc[cc.len() / 2..]
            .iter()
            .cloned()
            .collect::<HashSet<char>>();

        let c = left.intersection(&right).next().unwrap();
        p1 += match c {
            'a'..='z' => *c as u32 - 'a' as u32 + 1,
            'A'..='Z' => *c as u32 - 'A' as u32 + 27,
            _ => 0,
        };
    }
    println!("p1: {p1}");

    let mut p2 = 0;
    let lines: Vec<HashSet<char>> = lines
        .iter()
        .map(|l| l.chars().collect::<HashSet<char>>())
        .collect();
    for chunk in lines.chunks(3) {
        let first = chunk[0]
            .intersection(&chunk[1])
            .cloned()
            .collect::<HashSet<char>>();
        let c = first.intersection(&chunk[2]).next().unwrap();

        p2 += match c {
            'a'..='z' => *c as u32 - 'a' as u32 + 1,
            'A'..='Z' => *c as u32 - 'A' as u32 + 27,
            _ => 0,
        };
    }

    println!("p2: {p2}")
}
