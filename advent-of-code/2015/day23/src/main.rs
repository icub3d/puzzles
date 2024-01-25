use nom::{
    branch::alt, bytes::complete::tag, character::complete::one_of, combinator::map,
    sequence::tuple, IResult,
};

enum Instruction {
    Half(usize),
    Triple(usize),
    Increment(usize),
    Jump(i32),
    JumpIfEven(usize, i32),
    JumpIfOne(usize, i32),
}

impl Instruction {
    fn parse(input: &str) -> IResult<&str, Instruction> {
        let (input, instruction) = alt((
            map(tuple((tag("hlf "), one_of("ab"))), |(_, c)| {
                Instruction::Half(match c {
                    'a' => 0,
                    _ => 1,
                })
            }),
            map(tuple((tag("tpl "), one_of("ab"))), |(_, c)| {
                Instruction::Triple(match c {
                    'a' => 0,
                    _ => 1,
                })
            }),
            map(tuple((tag("inc "), one_of("ab"))), |(_, c)| {
                Instruction::Increment(match c {
                    'a' => 0,
                    _ => 1,
                })
            }),
            map(
                tuple((tag("jmp "), nom::character::complete::i32)),
                |(_, i)| Instruction::Jump(i),
            ),
            map(
                tuple((
                    tag("jie "),
                    one_of("ab"),
                    tag(", "),
                    nom::character::complete::i32,
                )),
                |(_, c, _, i)| {
                    Instruction::JumpIfEven(
                        match c {
                            'a' => 0,
                            _ => 1,
                        },
                        i,
                    )
                },
            ),
            map(
                tuple((
                    tag("jio "),
                    one_of("ab"),
                    tag(", "),
                    nom::character::complete::i32,
                )),
                |(_, c, _, i)| {
                    Instruction::JumpIfOne(
                        match c {
                            'a' => 0,
                            _ => 1,
                        },
                        i,
                    )
                },
            ),
        ))(input)?;
        Ok((input, instruction))
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let instructions: Vec<Instruction> = input
        .lines()
        .map(|line| Instruction::parse(line).unwrap().1)
        .collect();
    println!("p1: {}", run(&instructions, 0));
    println!("p2: {}", run(&instructions, 1));
}

fn run(instructions: &[Instruction], a: usize) -> usize {
    let mut registers = [a, 0];
    let mut pc = 0;
    while pc < instructions.len() {
        match instructions[pc] {
            Instruction::Half(r) => {
                registers[r] /= 2;
                pc += 1;
            }
            Instruction::Triple(r) => {
                registers[r] *= 3;
                pc += 1;
            }
            Instruction::Increment(r) => {
                registers[r] += 1;
                pc += 1;
            }
            Instruction::Jump(i) => {
                pc = (pc as i32 + i) as usize;
            }
            Instruction::JumpIfEven(r, i) => {
                if registers[r] % 2 == 0 {
                    pc = (pc as i32 + i) as usize;
                } else {
                    pc += 1;
                }
            }
            Instruction::JumpIfOne(r, i) => {
                if registers[r] == 1 {
                    pc = (pc as i32 + i) as usize;
                } else {
                    pc += 1;
                }
            }
        }
    }
    registers[1]
}
