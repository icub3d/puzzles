use std::{
    collections::HashSet,
    ops::{Add, Mul},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point::new(self.x + other.x, self.y + other.y)
    }
}

impl Mul<i32> for Point {
    type Output = Point;

    fn mul(self, other: i32) -> Point {
        Point::new(self.x * other, self.y * other)
    }
}

impl Point {
    fn manhattan_distance(&self) -> i32 {
        self.x.abs() + self.y.abs()
    }

    fn turn(&self, direction: char) -> Point {
        match direction {
            'R' => Point::new(self.y, -self.x),
            'L' => Point::new(-self.y, self.x),
            _ => panic!("Unknown direction {}", direction),
        }
    }

    fn walk(&self, delta: Point, distance: i32) -> Vec<Point> {
        (1..=distance).map(|i| *self + delta * i).collect()
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let moves = input
        .trim()
        .split(", ")
        .map(|s| (s.chars().next().unwrap(), s[1..].parse::<i32>().unwrap()))
        .collect::<Vec<_>>();

    let mut cur = Point::new(0, 0);
    let mut delta = Point::new(0, -1);
    for (turn, dist) in &moves {
        delta = delta.turn(*turn);
        cur = cur + delta * *dist;
    }
    println!("p1: {}", cur.manhattan_distance());

    let mut cur = Point::new(0, 0);
    let mut delta = Point::new(0, -1);
    let mut visited = HashSet::new();
    visited.insert(cur);
    for (turn, dist) in moves {
        delta = delta.turn(turn);
        for p in cur.walk(delta, dist) {
            if !visited.insert(p) {
                println!("p2: {}", p.manhattan_distance());
                return;
            }
        }
        cur = cur + delta * dist;
    }
}
