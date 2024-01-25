use std::{
    collections::{HashSet, VecDeque},
    ops::Add,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Point::new(self.x + other.x, self.y + other.y)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Beam {
    position: Point,
    direction: Direction,
}

impl Beam {
    fn new(position: Point, direction: Direction) -> Self {
        Beam {
            position,
            direction,
        }
    }

    fn out_of_bounds(&self, grid: &Vec<Vec<char>>) -> bool {
        self.position.x < 0
            || self.position.y < 0
            || self.position.x >= grid[0].len() as i32
            || self.position.y >= grid.len() as i32
    }

    fn move_forward(&mut self) {
        match self.direction {
            Direction::Up => self.position.y -= 1,
            Direction::Down => self.position.y += 1,
            Direction::Left => self.position.x -= 1,
            Direction::Right => self.position.x += 1,
        }
    }
}

struct Grid {
    grid: Vec<Vec<char>>,
}

impl Grid {
    fn parse(input: &str) -> Self {
        let grid = input
            .lines()
            .map(|line| line.chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        Grid { grid }
    }

    fn calculate_energized(&self, initial_beam: Beam) -> usize {
        // Track the number of points on the grid we've touched.
        let mut energized = HashSet::new();

        // Wehn we split, we need to keep track of beams we haven't
        // processed yet.
        let mut beams = VecDeque::new();
        beams.push_back(initial_beam);

        // Beams can cycle, so we need to track previous beams. This
        // differes
        let mut seen = HashSet::new();

        while let Some(mut beam) = beams.pop_front() {
            while !beam.out_of_bounds(&self.grid) && !seen.contains(&beam) {
                // Energize our position.
                energized.insert(beam.position.clone());
                seen.insert(beam.clone());

                // Determine how to move based on the current position.
                match (
                    self.grid[beam.position.y as usize][beam.position.x as usize],
                    &beam.direction,
                ) {
                    // If we hit a | going right or left, we want to make
                    // a copy going up and the move down.
                    ('|', Direction::Right | Direction::Left) => {
                        // Create a copy going up.
                        let mut up = beam.clone();
                        up.direction = Direction::Up;
                        up.move_forward();
                        beams.push_back(up);

                        beam.direction = Direction::Down;
                    }
                    // If we hit a - going up or down, we want to make
                    // a copy going left and the move right.
                    ('-', Direction::Up | Direction::Down) => {
                        // Create a copy going left.
                        let mut left = beam.clone();
                        left.direction = Direction::Left;
                        left.move_forward();
                        beams.push_back(left);

                        beam.direction = Direction::Right;
                    }
                    // The rest of the cases are just changing direction.
                    ('/', Direction::Up) => beam.direction = Direction::Right,
                    ('/', Direction::Down) => beam.direction = Direction::Left,
                    ('/', Direction::Left) => beam.direction = Direction::Down,
                    ('/', Direction::Right) => beam.direction = Direction::Up,
                    ('\\', Direction::Up) => beam.direction = Direction::Left,
                    ('\\', Direction::Down) => beam.direction = Direction::Right,
                    ('\\', Direction::Left) => beam.direction = Direction::Up,
                    ('\\', Direction::Right) => beam.direction = Direction::Down,

                    // Anything else is just moving forward.
                    _ => {}
                }
                // All of the above will cause the beam to move
                // forward in it's (new) direction.
                beam.move_forward();
            }
        }
        energized.len()
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let grid = Grid::parse(&input);

    // For part one, we want to find the max energized if we start on
    // the top left.
    let now = std::time::Instant::now();
    let initial_beam = Beam::new(Point::new(0, 0), Direction::Right);
    println!(
        "p1: {} ({:?})",
        grid.calculate_energized(initial_beam),
        now.elapsed()
    );

    // For part two, we want to find the max energized if we start on
    // all of the edges.
    let now = std::time::Instant::now();
    let mut max_energized = 0;
    // Do the top and bottom edges.
    for x in 0..grid.grid[0].len() {
        let beam = Beam::new(Point::new(x as i32, 0), Direction::Down);
        max_energized = max_energized.max(grid.calculate_energized(beam));
        let beam = Beam::new(
            Point::new(x as i32, grid.grid.len() as i32 - 1),
            Direction::Up,
        );
        max_energized = max_energized.max(grid.calculate_energized(beam));
    }
    // Do the left and right edges.
    for y in 0..grid.grid.len() {
        let beam = Beam::new(Point::new(0, y as i32), Direction::Right);
        max_energized = max_energized.max(grid.calculate_energized(beam));
        let beam = Beam::new(
            Point::new(grid.grid[0].len() as i32 - 1, y as i32),
            Direction::Left,
        );
        max_energized = max_energized.max(grid.calculate_energized(beam));
    }
    println!("p2: {} ({:?})", max_energized, now.elapsed());
}
