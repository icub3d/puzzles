use std::ops::Add;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map_res, opt},
    multi::separated_list1,
    IResult,
};

#[derive(Debug, Clone, Copy)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(x: usize, y: usize) -> Self {
        Point {
            x: x as isize,
            y: y as isize,
        }
    }

    fn parse_button(input: &str) -> IResult<&str, Self> {
        // Strip the Button A: and Button B: prefixes
        let (input, _) = alt((tag("Button A: X+"), tag("Button B: X+")))(input)?;

        let (input, x) = map_res(digit1, |s: &str| s.parse::<usize>())(input)?;
        let (input, _) = tag(", Y+")(input)?;
        let (input, y) = map_res(digit1, |s: &str| s.parse::<usize>())(input)?;
        let (input, _) = alt((tag("\n"), tag("\r\n")))(input)?;

        Ok((input, Point::new(x, y)))
    }

    fn parse_prize(input: &str) -> IResult<&str, Self> {
        let (input, _) = tag("Prize: X=")(input)?;

        let (input, x) = map_res(digit1, |s: &str| s.parse::<usize>())(input)?;
        let (input, _) = tag(", Y=")(input)?;
        let (input, y) = map_res(digit1, |s: &str| s.parse::<usize>())(input)?;
        let (input, _) = opt(alt((tag("\n"), tag("\r\n"))))(input)?;

        Ok((input, Point::new(x, y)))
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

#[derive(Debug, Clone)]
struct Machine {
    a: Point,
    b: Point,
    prize: Point,
}

impl Machine {
    fn parse_all(input: &str) -> IResult<&str, Vec<Machine>> {
        let (input, machines) =
            separated_list1(alt((tag("\n"), tag("\r\n"))), Machine::parse)(input)?;
        Ok((input, machines))
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, a) = Point::parse_button(input)?;
        let (input, b) = Point::parse_button(input)?;
        let (input, prize) = Point::parse_prize(input)?;

        Ok((input, Machine { a, b, prize }))
    }

    fn solve(&self) -> Option<isize> {
        // This is just mostly to make sense of the math I did on the whiteboard.
        let x1 = self.a.x;
        let y1 = self.a.y;
        let x2 = self.b.x;
        let y2 = self.b.y;
        let x3 = self.prize.x;
        let y3 = self.prize.y;

        // Solve for b.
        let num = y1 * x3 - x1 * y3;
        let denom = y1 * x2 - x1 * y2;

        // Check for "no solution". Obviously we can't divide by zero and the
        // division must be exact because you either press the button or you
        // don't.
        if denom == 0 {
            return None;
        } else if num % denom != 0 {
            return None;
        }
        let b = num / denom;

        // Solve for a.
        let num = x3 - b * x2;
        let denom = x1;
        if denom == 0 {
            return None;
        } else if num % denom != 0 {
            return None;
        }
        let a = num / denom;

        Some(a * 3 + b)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");
    let (_, machines) = Machine::parse_all(input)?;

    println!("machines: {:?}", machines.len());

    let now = std::time::Instant::now();
    let p1 = machines.iter().filter_map(|m| m.solve()).sum::<isize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();
    let conversion_error = Point::new(10000000000000, 10000000000000);
    let p2 = machines
        .iter()
        .filter_map(|m| {
            let mut m = m.clone();
            m.prize = m.prize + conversion_error;
            m.solve()
        })
        .sum::<isize>();
    println!("p2: {} ({:?})", p2, now.elapsed());

    Ok(())
}
