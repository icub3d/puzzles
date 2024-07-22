use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1, line_ending},
    combinator::{map, opt},
    multi::many0,
    sequence::{pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone)]
enum Operand {
    Register(char),
    Value(isize),
}

fn parse_digit(input: &str) -> IResult<&str, isize> {
    map(
        pair(opt(tag("-")), digit1),
        |(n, s): (Option<&str>, &str)| match n {
            Some(_) => -s.parse::<isize>().unwrap(),
            None => s.parse().unwrap(),
        },
    )(input)
}

fn parse_operand(input: &str) -> IResult<&str, Operand> {
    alt((
        map(alpha1, |s: &str| {
            Operand::Register(s.chars().next().unwrap())
        }),
        map(parse_digit, Operand::Value),
    ))(input)
}

#[derive(Debug, Clone)]
enum Instruction {
    CopyValue(Operand, Operand),
    Increment(Operand),
    Decrement(Operand),
    JumpIfNotZero(Operand, Operand),
    Toggle(Operand),
    Nop,
    Multiply(char, char, char),
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        map(tuple((tag("tgl "), parse_operand)), |(_, operand)| {
            Instruction::Toggle(operand)
        }),
        map(
            tuple((tag("cpy "), parse_operand, tag(" "), parse_operand)),
            |(_, operand, _, register)| Instruction::CopyValue(operand, register),
        ),
        map(tuple((tag("inc "), parse_operand)), |(_, register)| {
            Instruction::Increment(register)
        }),
        map(tuple((tag("dec "), parse_operand)), |(_, register)| {
            Instruction::Decrement(register)
        }),
        map(
            tuple((tag("jnz "), parse_operand, tag(" "), parse_operand)),
            |(_, operand, _, offset)| Instruction::JumpIfNotZero(operand, offset),
        ),
        map(tag("nop"), |_| Instruction::Nop),
        map(
            tuple((tag("mul "), alpha1, tag(" "), alpha1, tag(" "), alpha1)),
            |(_, a, _, b, _, c): (&str, &str, &str, &str, &str, &str)| {
                Instruction::Multiply(
                    a.chars().next().unwrap(),
                    b.chars().next().unwrap(),
                    c.chars().next().unwrap(),
                )
            },
        ),
    ))(input)
}

fn parse_instructions(input: &str) -> IResult<&str, Vec<Instruction>> {
    many0(terminated(parse_instruction, line_ending))(input)
}

fn main() {
    let input = include_str!("../input");
    let (_, instructions) = parse_instructions(input).unwrap();
    println!("p1: {}", run(&instructions, &[7, 0, 0, 0]));

    let input = include_str!("../input");
    let (_, instructions) = parse_instructions(input).unwrap();
    println!("p2: {}", run(&instructions, &[12, 0, 0, 0]));
}

fn run(instructions: &[Instruction], registers: &[isize; 4]) -> isize {
    let mut registers = registers.to_vec();
    let mut instructions = instructions.to_vec();
    let mut pc = 0;

    while pc < instructions.len() {
        match &instructions[pc] {
            Instruction::Multiply(a, b, c) => {
                let a = registers[*a as usize - 'a' as usize];
                let b = registers[*b as usize - 'a' as usize];
                registers[*c as usize - 'a' as usize] = a * b;
                pc += 1;
            }
            Instruction::CopyValue(value, Operand::Register(register)) => {
                let value = match value {
                    Operand::Register(register) => registers[*register as usize - 'a' as usize],
                    Operand::Value(value) => *value,
                };
                registers[*register as usize - 'a' as usize] = value;
                pc += 1;
            }
            Instruction::Increment(Operand::Register(register)) => {
                registers[*register as usize - 'a' as usize] += 1;
                pc += 1;
            }
            Instruction::Decrement(Operand::Register(register)) => {
                registers[*register as usize - 'a' as usize] -= 1;
                pc += 1;
            }
            Instruction::JumpIfNotZero(value, offset) => {
                let value = match value {
                    Operand::Register(register) => registers[*register as usize - 'a' as usize],
                    Operand::Value(value) => *value,
                };
                let offset = match offset {
                    Operand::Register(register) => registers[*register as usize - 'a' as usize],
                    Operand::Value(value) => *value,
                };
                if value != 0 {
                    pc = (pc as isize + offset) as usize;
                } else {
                    pc += 1;
                }
            }
            Instruction::Toggle(value) => {
                let value = match value {
                    Operand::Register(register) => registers[*register as usize - 'a' as usize],
                    Operand::Value(value) => *value,
                };
                let target = pc as isize + value;
                if target >= 0 && target < instructions.len() as isize {
                    match &instructions[target as usize] {
                        Instruction::CopyValue(src, dst) => {
                            instructions[target as usize] =
                                Instruction::JumpIfNotZero(src.clone(), dst.clone());
                        }
                        Instruction::JumpIfNotZero(operand, offset) => {
                            instructions[target as usize] =
                                Instruction::CopyValue(operand.clone(), offset.clone());
                        }
                        Instruction::Increment(register) => {
                            instructions[target as usize] =
                                Instruction::Decrement(register.clone());
                        }
                        Instruction::Decrement(register) => {
                            instructions[target as usize] =
                                Instruction::Increment(register.clone());
                        }
                        Instruction::Toggle(operand) => {
                            instructions[target as usize] = Instruction::Increment(operand.clone());
                        }
                        _ => {}
                    }
                }
                pc += 1;
            }
            _ => {
                // Skip nop and invalid
                pc += 1;
            }
        }
    }

    registers[0]
}
