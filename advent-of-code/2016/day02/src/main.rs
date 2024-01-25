use std::collections::HashMap;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum Instruction {
    Up,
    Down,
    Left,
    Right,
}

impl From<char> for Instruction {
    fn from(c: char) -> Self {
        match c {
            'U' => Instruction::Up,
            'D' => Instruction::Down,
            'L' => Instruction::Left,
            'R' => Instruction::Right,
            _ => panic!("Invalid instruction"),
        }
    }
}

struct KeyPad {
    graph: HashMap<char, HashMap<Instruction, char>>,
}

impl KeyPad {
    fn from(input: &str) -> Self {
        let mut graph = HashMap::new();
        for line in input.lines() {
            let mut parts = line.split_whitespace().collect::<Vec<_>>();
            let key = parts[0].chars().next().unwrap();
            let mut edges = HashMap::new();
            for part in parts[1..].iter_mut() {
                let mut chars = part.chars();
                let instruction = chars.next().unwrap().into();
                let value = chars.next().unwrap();
                edges.insert(instruction, value);
            }
            graph.insert(key, edges);
        }
        KeyPad { graph }
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let directions = input
        .lines()
        .map(|line| line.chars().collect())
        .collect::<Vec<Vec<char>>>();

    let keypad = KeyPad::from(std::fs::read_to_string("edges1").unwrap().as_str());
    let mut cur = '5';
    let mut code = String::new();
    for direction in directions.clone() {
        for instruction in direction {
            cur = match keypad.graph.get(&cur).unwrap().get(&instruction.into()) {
                Some(&c) => c,
                None => cur,
            }
        }
        code.push(cur);
    }
    println!("p1: {}", code);

    let keypad = KeyPad::from(std::fs::read_to_string("edges2").unwrap().as_str());
    let mut cur = '5';
    let mut code = String::new();
    for direction in directions {
        for instruction in direction {
            cur = match keypad.graph.get(&cur).unwrap().get(&instruction.into()) {
                Some(&c) => c,
                None => cur,
            }
        }
        code.push(cur);
    }
    println!("p1: {}", code);
}
