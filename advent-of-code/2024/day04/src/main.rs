use std::collections::HashMap;

use itertools::Itertools;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
struct Point {
    x: isize,
    y: isize,
}

// Implement Add/Sub/Mul for Point so we can do math with them (For all you C++
// operator overloading fans)

impl std::ops::Add<Point> for &Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::ops::Add<&Point> for Point {
    type Output = Self;

    fn add(self, other: &Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl std::ops::Sub<&Point> for &Point {
    type Output = Point;

    fn sub(self, other: &Point) -> Point {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl std::ops::Mul<isize> for Point {
    type Output = Point;

    fn mul(self, other: isize) -> Point {
        Point {
            x: self.x * other,
            y: self.y * other,
        }
    }
}

impl Point {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }
}

struct Puzzle {
    grid: HashMap<Point, char>,
}

impl Puzzle {
    fn new(input: &str) -> Self {
        // We can simply iterate over the lines and characters and create a
        // hashmap of points to characters. Note we we use zip() here instead of
        // enumerate() because we are using isize for the coordinates.
        let grid = input
            .lines()
            .zip(0_isize..)
            .flat_map(|(line, y)| {
                line.chars()
                    .zip(0_isize..)
                    .map(move |(c, x)| (Point::new(x, y), c))
            })
            .collect::<HashMap<Point, char>>();
        Self { grid }
    }

    // Get returns the value at a point, or '.' if the point is out of bounds.
    fn get(&self, point: Point) -> char {
        match self.grid.get(&point) {
            Some(c) => *c,
            None => '.',
        }
    }

    // Find all of the neighbors of a point that are in the grid.
    fn neighbors(&self, point: &Point) -> Vec<Point> {
        (-1..=1)
            .cartesian_product(-1..=1)
            .filter(|(dx, dy)| *dx != 0 || *dy != 0)
            .map(|(dx, dy)| Point::new(point.x + dx, point.y + dy))
            .filter(|p| self.grid.contains_key(p))
            .collect()
    }

    fn p1(&self) -> usize {
        self.grid
            .iter()
            .filter(|(_, c)| **c == 'X') // Look for an X.
            .map(|(point, _)| {
                self.neighbors(point)
                    .into_iter()
                    .filter(|p| self.get(*p) == 'M') // Look for an adjacent M.
                    .filter(|p| {
                        // Figure out the direction the M is going out from the
                        // X and then look for the A and S in that same
                        // direction.
                        let direction = p - point;
                        let a = direction * 2 + point;
                        let s = direction * 3 + point;
                        match (self.get(a), self.get(s)) {
                            ('A', 'S') => true,
                            _ => false,
                        }
                    })
                    .count()
            })
            .sum()
    }

    fn p2(&self) -> usize {
        self.grid
            .iter()
            .filter(|(_, c)| **c == 'A')
            .filter(|(p, _)| {
                // Get the diagonal neighbors of the A.
                let top_left = self.get(*p + Point::new(-1, -1));
                let top_right = self.get(*p + Point::new(1, -1));
                let bottom_left = self.get(*p + Point::new(-1, 1));
                let bottom_right = self.get(*p + Point::new(1, 1));

                // Check if both diagonals have an S and M in them.
                ((top_left == 'S' && bottom_right == 'M')
                    || (top_left == 'M' && bottom_right == 'S'))
                    && ((top_right == 'S' && bottom_left == 'M')
                        || (top_right == 'M' && bottom_left == 'S'))
            })
            .count()
    }
}

fn main() {
    let input = include_str!("input.txt");

    let puzzle = Puzzle::new(input);
    println!("p1: {}", puzzle.p1());
    println!("p2: {}", puzzle.p2());
}
