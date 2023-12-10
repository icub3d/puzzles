use std::collections::HashMap;

use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, newline, one_of},
    multi::{many1, separated_list0},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct Edges<'a> {
    left: &'a str,
    right: &'a str,
}

#[derive(Debug)]
enum Direction {
    Left,
    Right,
}

impl Direction {
    fn new(c: char) -> Self {
        match c {
            'L' => Direction::Left,
            _ => Direction::Right,
        }
    }
}

#[derive(Debug)]
struct Map<'a> {
    instructions: Vec<Direction>,
    edges: HashMap<&'a str, Edges<'a>>,
}

impl Map<'_> {
    fn parse(input: &str) -> IResult<&str, Map> {
        let (input, instructions) = many1(one_of("LR"))(input)?;
        let instructions = instructions.iter().map(|c| Direction::new(*c)).collect();
        let (input, _) = many1(newline)(input)?;
        let mut edges = HashMap::new();
        let (input, nodes) = separated_list0(
            tag("\n"),
            tuple((alpha1, tag(" = ("), alpha1, tag(", "), alpha1, tag(")"))),
        )(input)?;
        nodes.iter().for_each(|(node, _, left, _, right, _)| {
            edges.insert(*node, Edges { left, right });
        });
        Ok((
            input,
            Map {
                instructions,
                edges,
            },
        ))
    }

    fn find_end<'a, F>(&'a self, start: &'a str, end: F) -> (usize, &str)
    where
        F: Fn(&str) -> bool,
    {
        let mut steps = 0;
        let mut cur = start;
        for dir in self.instructions.iter().cycle() {
            let edges = self.edges.get(cur).unwrap();
            cur = match dir {
                Direction::Left => edges.left,
                Direction::Right => edges.right,
            };
            steps += 1;
            if end(cur) {
                break;
            }
        }
        (steps, cur)
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let (_, map) = Map::parse(&input).unwrap();
    println!("p1: {}", map.find_end("AAA", |s| s == "ZZZ").0);

    let starts = map
        .edges
        .keys()
        .filter(|k| k.ends_with('A'))
        .collect::<Vec<_>>();
    let ends = map
        .edges
        .keys()
        .filter(|k| k.ends_with('Z'))
        .collect::<Vec<_>>();
    println!("starts: {:?}", starts);
    println!("ends: {:?}", ends);
    let mut steps = vec![];
    for start in starts {
        let (s, end) = map.find_end(start, |s| ends.contains(&&s));
        steps.push(s);
        println!("start: {}, steps: {}, end: {}", start, s, end);
    }
    let mut start = "ZZZ";
    for _ in 0..=9 {
        let (s, end) = map.find_end(start, |s| ends.contains(&&s));
        println!("test-zzz: {}, steps: {}, end: {}", start, s, end);
        start = end;
    }

    let p2 = lcm(&steps);
    println!("p2: {}", p2);
    for step in steps.iter() {
        println!("{}", p2 / step);
    }
}

// https://github.com/TheAlgorithms/Rust/blob/master/src/math/lcm_of_n_numbers.rs

pub fn lcm(nums: &[usize]) -> usize {
    if nums.len() == 1 {
        return nums[0];
    }
    let a = nums[0];
    let b = lcm(&nums[1..]);
    a * b / gcd_of_two_numbers(a, b)
}

fn gcd_of_two_numbers(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    gcd_of_two_numbers(b, a % b)
}
