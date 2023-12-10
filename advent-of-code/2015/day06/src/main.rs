#[derive(Debug, PartialEq, Eq)]
struct Coordinate {
    x: usize,
    y: usize,
}

impl From<&str> for Coordinate {
    fn from(s: &str) -> Self {
        let parts = s.split(',').collect::<Vec<&str>>();
        Self {
            x: parts[0].parse().unwrap(),
            y: parts[1].parse().unwrap(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Operation {
    TurnOn,
    TurnOff,
    Flip,
}

#[derive(Debug)]
struct Instruction {
    op: Operation,
    start: Coordinate,
    end: Coordinate,
}

impl From<&str> for Instruction {
    fn from(s: &str) -> Self {
        let parts = s.split(' ').collect::<Vec<&str>>();
        let parts = match parts.len() {
            5 => parts[1..].into(),
            _ => parts,
        };

        Self {
            op: match parts[0] {
                "on" => Operation::TurnOn,
                "off" => Operation::TurnOff,
                _ => Operation::Flip,
            },
            start: parts[1].into(),
            end: parts[3].into(),
        }
    }
}

fn main() {
    let lines = std::fs::read_to_string("input").unwrap();
    let instructions = lines
        .lines()
        .map(|l| l.into())
        .collect::<Vec<Instruction>>();
    let now = std::time::Instant::now();
    let mut grid: Vec<Vec<bool>> = vec![vec![false; 1000]; 1000];
    for instruction in instructions.iter() {
        for x in instruction.start.x..=instruction.end.x {
            for y in instruction.start.y..=instruction.end.y {
                grid[x][y] = match instruction.op {
                    Operation::TurnOn => true,
                    Operation::TurnOff => false,
                    Operation::Flip => !grid[x][y],
                };
            }
        }
    }
    let lit = grid.iter().flatten().filter(|v| **v).count();
    println!("Part 1: {} ({:?})", lit, now.elapsed());

    let now = std::time::Instant::now();
    let mut grid: Vec<Vec<usize>> = vec![vec![0; 1000]; 1000];
    for instruction in instructions.iter() {
        for x in instruction.start.x..=instruction.end.x {
            for y in instruction.start.y..=instruction.end.y {
                grid[x][y] = match instruction.op {
                    Operation::TurnOn => grid[x][y] + 1,
                    Operation::TurnOff => match grid[x][y] {
                        0 => 0,
                        _ => grid[x][y] - 1,
                    },
                    Operation::Flip => grid[x][y] + 2,
                };
            }
        }
    }
    let lit: usize = grid.iter().flatten().sum();
    println!("Part 2: {} ({:?})", lit, now.elapsed());
}
