use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit1};
use nom::combinator::{map, opt};
use nom::multi::separated_list1;
use nom::sequence::pair;
use nom::IResult;

enum Operation {
    Increment,
    Decrement,
}

impl Operation {
    fn parse(input: &str) -> IResult<&str, Operation> {
        alt((
            map(tag("inc"), |_| Operation::Increment),
            map(tag("dec"), |_| Operation::Decrement),
        ))(input)
    }

    fn apply(&self, left: i32, right: i32) -> i32 {
        match self {
            Operation::Increment => left + right,
            Operation::Decrement => left - right,
        }
    }
}

enum Comparison {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl Comparison {
    fn parse(input: &str) -> IResult<&str, Comparison> {
        alt((
            map(tag("=="), |_| Comparison::Equal),
            map(tag("!="), |_| Comparison::NotEqual),
            map(tag(">="), |_| Comparison::GreaterThanOrEqual),
            map(tag("<="), |_| Comparison::LessThanOrEqual),
            map(tag(">"), |_| Comparison::GreaterThan),
            map(tag("<"), |_| Comparison::LessThan),
        ))(input)
    }

    fn compare(&self, left: i32, right: i32) -> bool {
        match self {
            Comparison::Equal => left == right,
            Comparison::NotEqual => left != right,
            Comparison::GreaterThan => left > right,
            Comparison::GreaterThanOrEqual => left >= right,
            Comparison::LessThan => left < right,
            Comparison::LessThanOrEqual => left <= right,
        }
    }
}

fn parse_digit(input: &str) -> IResult<&str, i32> {
    map(
        pair(opt(tag("-")), digit1),
        |(n, s): (Option<&str>, &str)| match n {
            Some(_) => -s.parse::<i32>().unwrap(),
            None => s.parse().unwrap(),
        },
    )(input)
}

struct Instruction {
    register: String,
    operation: Operation,
    value: i32,
    condition_register: String,
    comparison: Comparison,
    condition_value: i32,
}

impl Instruction {
    fn parse_all(input: &str) -> IResult<&str, Vec<Instruction>> {
        separated_list1(tag("\n"), Instruction::parse)(input)
    }

    fn parse(input: &str) -> IResult<&str, Instruction> {
        let (input, register) = alpha1(input)?;
        let (input, _) = tag(" ")(input)?;
        let (input, operation) = Operation::parse(input)?;
        let (input, _) = tag(" ")(input)?;
        let (input, value) = parse_digit(input)?;
        let (input, _) = tag(" if ")(input)?;
        let (input, condition_register) = alpha1(input)?;
        let (input, _) = tag(" ")(input)?;
        let (input, comparison) = Comparison::parse(input)?;
        let (input, _) = tag(" ")(input)?;
        let (input, condition_value) = parse_digit(input)?;

        Ok((
            input,
            Instruction {
                register: register.to_string(),
                operation,
                value,
                condition_register: condition_register.to_string(),
                comparison,
                condition_value,
            },
        ))
    }
}

fn main() {
    let input = include_str!("../input");
    let instructions = Instruction::parse_all(input).unwrap().1;

    let mut registers = HashMap::new();
    let mut max_value = 0;
    for instruction in instructions {
        let condition_register = registers
            .entry(instruction.condition_register.clone())
            .or_insert(0);
        if instruction
            .comparison
            .compare(*condition_register, instruction.condition_value)
        {
            let register = registers.entry(instruction.register.clone()).or_insert(0);
            *register = instruction.operation.apply(*register, instruction.value);
            max_value = max_value.max(*register);
        }
    }

    let max = registers.values().max().unwrap();
    println!("p1: {}", max);
    println!("p2: {}", max_value);
}
