use rayon::prelude::*;

use std::collections::VecDeque;

use nom::{
    bytes::complete::tag, character::complete::digit1, combinator::map_res, multi::separated_list1,
    IResult,
};

#[derive(Debug)]
struct Test {
    calibration: u64,
    values: VecDeque<u64>,
}

impl Test {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, calibration) = map_res(digit1, |s: &str| s.parse::<u64>())(input)?;
        let (input, _) = tag(": ")(input)?;
        let (input, values) =
            separated_list1(tag(" "), map_res(digit1, |s: &str| s.parse::<u64>()))(input)?;
        Ok((
            input,
            Self {
                calibration,
                values: values.into(),
            },
        ))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");
    let tests: Vec<Test> = input
        .lines()
        .map(|line| Test::parse(line).map(|(_, test)| test))
        .collect::<Result<Vec<_>, _>>()?;

    let now = std::time::Instant::now();
    let p1 = tests
        .iter()
        .filter(|test| is_valid(false, test.calibration, &mut test.values.clone()))
        .map(|test| test.calibration)
        .sum::<u64>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();
    let p2 = tests
        .par_iter()
        .filter(|test| is_valid(true, test.calibration, &mut test.values.clone()))
        .map(|test| test.calibration)
        .sum::<u64>();
    println!("p2: {} ({:?})", p2, now.elapsed());
    Ok(())
}

fn is_valid(concat: bool, calibration: u64, values: &mut VecDeque<u64>) -> bool {
    if values.len() == 0 {
        return false;
    } else if values.len() == 1 {
        return values[0] == calibration;
    } else if values[0] > calibration {
        return false;
    }

    // Try adding values[0] and values[1] together and seeing if that's valid.
    let left = values.pop_front().unwrap();
    let right = values.pop_front().unwrap();
    values.push_front(left + right);
    if is_valid(concat, calibration, values) {
        return true;
    }

    values.pop_front();
    values.push_front(left * right);
    if is_valid(concat, calibration, values) {
        return true;
    }

    // If we're allowed to concatenate, try concatenating the two values.
    if concat {
        values.pop_front();
        let concatenated = format!("{}{}", left, right).parse().unwrap();
        values.push_front(concatenated);
        if is_valid(concat, calibration, values) {
            return true;
        }
    }

    // If we didn't find a valid solution, we need to put the values back because
    // The call above will expect to have the original values.
    values.pop_front();
    values.push_front(right);
    values.push_front(left);
    false
}
