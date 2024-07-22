use std::collections::VecDeque;

enum Instruction {
    SwapPositions(usize, usize),
    SwapLetters(char, char),
    RotateLeft(usize),
    RotateRight(usize),
    RotateBasedOnLetter(char),
    Reverse(usize, usize),
    Move(usize, usize),
}

impl Instruction {
    fn from_str(s: &str) -> Instruction {
        let words: Vec<&str> = s.split_whitespace().collect();
        match words[0] {
            "swap" => match words[1] {
                "position" => {
                    let x = words[2].parse().unwrap();
                    let y = words[5].parse().unwrap();
                    Instruction::SwapPositions(x, y)
                }
                "letter" => {
                    let x = words[2].chars().next().unwrap();
                    let y = words[5].chars().next().unwrap();
                    Instruction::SwapLetters(x, y)
                }
                _ => panic!("Invalid swap instruction"),
            },
            "rotate" => match words[1] {
                "left" => {
                    let x = words[2].parse().unwrap();
                    Instruction::RotateLeft(x)
                }
                "right" => {
                    let x = words[2].parse().unwrap();
                    Instruction::RotateRight(x)
                }
                "based" => {
                    let x = words[6].chars().next().unwrap();
                    Instruction::RotateBasedOnLetter(x)
                }
                _ => panic!("Invalid rotate instruction"),
            },
            "reverse" => {
                let x = words[2].parse().unwrap();
                let y = words[4].parse().unwrap();
                Instruction::Reverse(x, y)
            }
            "move" => {
                let x = words[2].parse().unwrap();
                let y = words[5].parse().unwrap();
                Instruction::Move(x, y)
            }
            _ => panic!("Invalid instruction"),
        }
    }
}

fn main() {
    let input = include_str!("../input");
    let instructions = input
        .lines()
        .map(Instruction::from_str)
        .collect::<Vec<Instruction>>();

    let word = "abcdefgh";
    let mut letters = word.chars().collect::<VecDeque<char>>();
    for instruction in instructions.iter() {
        scramble(&mut letters, instruction, false);
    }
    let word = letters.iter().collect::<String>();
    println!("p1: {}", word);

    let word = "fbgdceah";
    let mut letters = word.chars().collect::<VecDeque<char>>();
    for instruction in instructions.iter().rev() {
        scramble(&mut letters, instruction, true);
    }
    let word = letters.iter().collect::<String>();
    println!("p2: {}", word);
}

fn scramble(letters: &mut VecDeque<char>, instruction: &Instruction, reverse: bool) {
    match (instruction, reverse) {
        (Instruction::SwapPositions(x, y), _) => {
            letters.swap(*x, *y);
        }
        (Instruction::SwapLetters(x, y), _) => {
            let x = letters.iter().position(|&c| c == *x).unwrap();
            let y = letters.iter().position(|&c| c == *y).unwrap();
            letters.swap(x, y);
        }
        (Instruction::RotateLeft(x), false) | (Instruction::RotateRight(x), true) => {
            for _ in 0..*x {
                let c = letters.pop_front().unwrap();
                letters.push_back(c);
            }
        }
        (Instruction::RotateRight(x), false) | (Instruction::RotateLeft(x), true) => {
            for _ in 0..*x {
                let c = letters.pop_back().unwrap();
                letters.push_front(c);
            }
        }
        (Instruction::RotateBasedOnLetter(x), false) => {
            let x = letters.iter().position(|&c| c == *x).unwrap();
            let x = if x >= 4 { x + 2 } else { x + 1 };
            for _ in 0..x {
                let c = letters.pop_back().unwrap();
                letters.push_front(c);
            }
        }
        (Instruction::RotateBasedOnLetter(x), true) => {
            let x = letters.iter().position(|&c| c == *x).unwrap();
            let x = match x {
                0 => 1,
                1 => 1,
                2 => 6,
                3 => 2,
                4 => 7,
                5 => 3,
                6 => 0,
                7 => 4,
                _ => panic!("invalid index"),
            };
            for _ in 0..x {
                let c = letters.pop_front().unwrap();
                letters.push_back(c);
            }
        }
        (Instruction::Reverse(x, y), _) => {
            let mut x = *x;
            let mut y = *y;
            while x < y {
                letters.swap(x, y);
                x += 1;
                y -= 1;
            }
        }
        (Instruction::Move(x, y), false) => {
            let c = letters.remove(*x).unwrap();
            letters.insert(*y, c);
        }
        (Instruction::Move(x, y), true) => {
            let c = letters.remove(*y).unwrap();
            letters.insert(*x, c);
        }
    }
}
