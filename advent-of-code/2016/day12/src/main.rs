use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1, line_ending},
    combinator::{map, opt},
    multi::many0,
    sequence::{pair, terminated, tuple},
    IResult,
};

#[derive(Debug)]
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

#[derive(Debug)]
enum Instruction {
    CopyValue(Operand, char),
    Increment(char),
    Decrement(char),
    JumpIfNotZero(Operand, isize),
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        map(
            tuple((tag("cpy "), parse_operand, tag(" "), alpha1)),
            |(_, operand, _, register)| {
                Instruction::CopyValue(operand, register.chars().next().unwrap())
            },
        ),
        map(
            tuple((tag("inc "), alpha1)),
            |(_, register): (&str, &str)| Instruction::Increment(register.chars().next().unwrap()),
        ),
        map(
            tuple((tag("dec "), alpha1)),
            |(_, register): (&str, &str)| Instruction::Decrement(register.chars().next().unwrap()),
        ),
        map(
            tuple((tag("jnz "), parse_operand, tag(" "), parse_digit)),
            |(_, operand, _, offset)| Instruction::JumpIfNotZero(operand, offset),
        ),
    ))(input)
}

fn parse_instructions(input: &str) -> IResult<&str, Vec<Instruction>> {
    many0(terminated(parse_instruction, line_ending))(input)
}

fn main() {
    let input = include_str!("../input");
    let (_, instructions) = parse_instructions(input).unwrap();

    println!("p1: {}", run(&instructions, 0));
    println!("p2: {}", run(&instructions, 1));
}

fn run(instructions: &[Instruction], c: isize) -> isize {
    let mut registers = [0; 4];
    registers[2] = c;
    let mut pc = 0;

    while pc < instructions.len() {
        match instructions[pc] {
            Instruction::CopyValue(Operand::Value(value), register) => {
                registers[register as usize - 'a' as usize] = value;
                pc += 1;
            }
            Instruction::CopyValue(Operand::Register(src), dst) => {
                registers[dst as usize - 'a' as usize] = registers[src as usize - 'a' as usize];
                pc += 1;
            }
            Instruction::Increment(register) => {
                registers[register as usize - 'a' as usize] += 1;
                pc += 1;
            }
            Instruction::Decrement(register) => {
                registers[register as usize - 'a' as usize] -= 1;
                pc += 1;
            }
            Instruction::JumpIfNotZero(Operand::Value(value), offset) => {
                if value != 0 {
                    pc = (pc as isize + offset) as usize;
                } else {
                    pc += 1;
                }
            }
            Instruction::JumpIfNotZero(Operand::Register(register), offset) => {
                if registers[register as usize - 'a' as usize] != 0 {
                    pc = (pc as isize + offset) as usize;
                } else {
                    pc += 1;
                }
            }
        }
    }

    registers[0]
}
