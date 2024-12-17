use fxhash::FxHashSet;
use rayon::prelude::*;

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn turn_right(&self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::South => Direction::West,
            Direction::East => Direction::South,
            Direction::West => Direction::North,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(x: isize, y: isize) -> Point {
        Point { x, y }
    }

    fn next(&self, direction: &Direction) -> Point {
        match direction {
            Direction::North => Point::new(self.x, self.y - 1),
            Direction::South => Point::new(self.x, self.y + 1),
            Direction::East => Point::new(self.x + 1, self.y),
            Direction::West => Point::new(self.x - 1, self.y),
        }
    }
}

#[derive(Debug, Clone)]
struct Map {
    positions: HashMap<Point, char>,
}

impl Map {
    fn new(input: &str) -> (Map, Point) {
        let mut current_position = Point::new(0, 0);
        let mut positions = HashMap::new();
        for (line, y) in input.lines().zip(0_isize..) {
            for (mut c, x) in line.chars().zip(0_isize..) {
                if c == '^' {
                    current_position = Point::new(x, y);
                    c = '.';
                }
                positions.insert(Point::new(x, y), c);
            }
        }
        (Map { positions }, current_position)
    }

    fn get(&self, point: &Point) -> Option<char> {
        self.positions.get(point).copied()
    }
}

fn main() {
    let input = include_str!("input.txt");
    let (map, mut current_position) = Map::new(input);
    let mut direction = Direction::North;
    let original_position = current_position;

    let mut directions: HashMap<Point, Vec<Direction>> = HashMap::new();

    let mut seen = HashSet::new();
    seen.insert(current_position);
    loop {
        directions
            .entry(current_position)
            .or_default()
            .push(direction);

        // Try to move our current direction. If we can't, turn right.
        let next_position = current_position.next(&direction);
        match map.get(&next_position) {
            Some('#') => {
                direction = direction.turn_right();
            }
            None => break,
            _ => {
                current_position = next_position;
                seen.insert(current_position);
            }
        }
    }
    println!("p1: {}", seen.len());

    println!(
        "{} / {}",
        directions.values().map(|v| v.len()).sum::<usize>(),
        map.positions.len()
    );

    let now = std::time::Instant::now();
    let obstacles = directions
        .par_iter()
        .flat_map(|(point, directions)| {
            directions
                .iter()
                .filter_map(|&direction| {
                    find_loop(&map, &original_position, &point.next(&direction))
                        .map(|_| point.next(&direction))
                })
                .collect::<Vec<_>>()
        })
        .collect::<HashSet<Point>>();
    println!("p2: {} ({:?})", obstacles.len(), now.elapsed());
}

fn find_loop(map: &Map, start: &Point, obstacle: &Point) -> Option<()> {
    // Assume we put an obstacle on the map at the next position of the
    // direction we are going.  We should then follow the path until we get back
    // to the same direction and point or go off the map.
    let mut map = map.clone();
    if map.get(&obstacle) == Some('.') {
        map.positions.insert(*obstacle, '#');
    } else {
        return None;
    }

    // Run the loop
    let mut current_position = *start;
    let mut current_direction = Direction::North;
    let mut seen = FxHashSet::default();
    loop {
        // Try to move our current direction. If we can't, turn right.
        let next_position = current_position.next(&current_direction);
        match map.get(&next_position) {
            Some('#') => {
                current_direction = current_direction.turn_right();
            }
            None => return None,
            _ => {
                current_position = next_position;
            }
        }
        if seen.contains(&(current_position, current_direction)) {
            return Some(());
        }
        seen.insert((current_position, current_direction));
    }
}
