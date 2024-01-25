use std::collections::HashSet;

use nom::{
    bytes::complete::tag,
    character::complete::{alphanumeric1, digit1, one_of, space1},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn parse(input: &str) -> IResult<&str, Direction> {
        let (input, direction) = one_of("UDLR")(input)?;
        let direction = match direction {
            'U' => Direction::Up,
            'D' => Direction::Down,
            'L' => Direction::Left,
            'R' => Direction::Right,
            _ => unreachable!(),
        };
        Ok((input, direction))
    }
}

#[derive(Debug)]
struct Instruction<'a> {
    direction: Direction,
    distance: u32,
    color: &'a str,
}

impl<'a> Instruction<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Instruction<'a>> {
        // R 6 (#999999)
        let (input, direction) = Direction::parse(input)?;
        let (input, _) = space1(input)?;
        let (input, distance) = digit1(input)?;
        let (input, _) = space1(input)?;
        let (input, (_, color, _)) = tuple((tag("(#"), alphanumeric1, tag(")")))(input)?;
        let distance = distance.parse().unwrap();
        Ok((
            input,
            Instruction {
                direction,
                distance,
                color,
            },
        ))
    }

    fn color_to_instruction(color: &str) -> Instruction {
        // The first 5 are hex digits, the last 1 is the direction.
        let (color, direction) = color.split_at(color.len() - 1);
        let direction = match direction {
            "3" => Direction::Up,
            "1" => Direction::Down,
            "2" => Direction::Left,
            "0" => Direction::Right,
            _ => unreachable!(),
        };
        let distance = u32::from_str_radix(color, 16).unwrap();
        Instruction {
            direction,
            distance,
            color,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn neighbors(&self) -> Vec<Point> {
        vec![
            Point {
                x: self.x + 1,
                y: self.y,
            },
            Point {
                x: self.x - 1,
                y: self.y,
            },
            Point {
                x: self.x,
                y: self.y + 1,
            },
            Point {
                x: self.x,
                y: self.y - 1,
            },
        ]
    }
}

struct Grid {
    grid: HashSet<Point>,
}

impl Grid {
    fn new() -> Self {
        Grid {
            grid: HashSet::new(),
        }
    }

    fn draw_edges(&mut self, instructions: &[Instruction]) {
        let mut cur = Point { x: 0, y: 0 };

        // Draw the edges.
        for instruction in instructions {
            for _ in 0..instruction.distance {
                match instruction.direction {
                    Direction::Up => cur.y += 1,
                    Direction::Down => cur.y -= 1,
                    Direction::Left => cur.x -= 1,
                    Direction::Right => cur.x += 1,
                }
                self.grid.insert(cur);
            }
        }
    }

    fn fill(&mut self, start: &Point) {
        // Mark my current point.
        self.grid.insert(*start);

        // Check all of my neighbors.
        for neighbor in start.neighbors() {
            // If the neighbor is already in the grid, then we don't need
            // to check it again as this might cause use to loop forever
            // or exit.
            if !self.grid.contains(&neighbor) {
                self.fill(&neighbor);
            }
        }
    }

    fn find_point_inside(&self) -> Option<Point> {
        // The intuition here is that some edge of the polygon along
        // the left side will be of size greater than 1. This won't
        // work in general cases where there is no spaces between the
        // trenches. along the left side.

        // Find the left-most x value.
        let min_x = self.grid.iter().map(|p| p.x).min().unwrap();

        // Iterate through all the points with that x value.
        let mut start = None;
        for p in self.grid.iter().filter(|p| p.x == min_x) {
            // Look for an empty point to the right of me.
            let right = Point { x: p.x + 1, y: p.y };
            if !self.grid.contains(&right) {
                start = Some(right);
                break;
            }
        }
        start
    }
}

fn vertices(instructions: &[Instruction]) -> (Vec<Point>, isize) {
    let mut cur = Point { x: 0, y: 0 };
    let mut perimeter = 0;
    let mut vertices = Vec::new();

    // Draw the edges.
    for instruction in instructions {
        let distance = instruction.distance as isize;
        match instruction.direction {
            Direction::Up => cur.y += distance,
            Direction::Down => cur.y -= distance,
            Direction::Left => cur.x -= distance,
            Direction::Right => cur.x += distance,
        }
        vertices.push(cur);
        perimeter += distance;
    }
    (vertices, perimeter)
}

fn shoelace(vertices: &[Point]) -> isize {
    // https://en.wikipedia.org/wiki/Shoelace_formula
    // Sum of the cross-products of neighboring vertices.
    vertices
        .iter()
        .zip(vertices.iter().cycle().skip(1))
        .map(|(a, b)| a.x * b.y - a.y * b.x)
        .sum::<isize>()
        .abs()
        / 2
}

fn calculate_area(vertices: &[Point], perimeter: isize) -> isize {
    // https://en.wikipedia.org/wiki/Pick%27s_theorem
    // A = i + b / 2 - 1
    //
    // This doesn't actually works though because it doesn't account
    // for the outside edges of the trench. Instead, we want to add
    // the outside edge area to the interior area.
    //
    // Each straight part of the edges accounts for 1/2. All move
    // inward (3/4) and outward (1/4) will cancel each other out, so
    // they are 1/2 as well. That just leaves the final corners of the
    // "rectangle" (since we know it's a closed circuit) to account
    // for an additional one.
    //
    // i = interior (shoelace)
    // perimeter / 2 + 1 = outside edges
    let i = shoelace(vertices);
    i + perimeter / 2 + 1
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let instructions = input
        .lines()
        .map(|line| Instruction::parse(line).unwrap().1)
        .collect::<Vec<_>>();

    // Part 1 - Use a simple algorithm to draw the edges, find a point
    // inside of it and then flood fill.
    let now = std::time::Instant::now();
    let mut grid = Grid::new();
    grid.draw_edges(&instructions);
    grid.fill(&grid.find_point_inside().unwrap());
    println!("p1: {} ({:?})", grid.grid.len(), now.elapsed());

    // Try part 1 with calculate_area.
    let now = std::time::Instant::now();
    let (vv, p) = vertices(&instructions);
    println!("p1-picks: {} ({:?})", calculate_area(&vv, p), now.elapsed());

    // Part 2 - We need new instructions and some math.
    let instructions = instructions
        .iter()
        .map(|i| Instruction::color_to_instruction(i.color))
        .collect::<Vec<_>>();
    let now = std::time::Instant::now();
    let (vv, p) = vertices(&instructions);
    println!("p2: {} ({:?})", calculate_area(&vv, p), now.elapsed());

    // Analysis
    // dbg!(&vv, p);
    let min_x = vv
        .iter()
        .map(|p| p.x)
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap();
    let max_x = vv
        .iter()
        .map(|p| p.x)
        .max_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap();
    let min_y = vv
        .iter()
        .map(|p| p.y)
        .min_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap();
    let max_y = vv
        .iter()
        .map(|p| p.y)
        .max_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap();
    dbg!(min_x, max_x, min_y, max_y);
}
