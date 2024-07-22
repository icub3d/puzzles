use std::{collections::HashMap, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

impl FromStr for Point {
    type Err = ();

    fn from_str(s: &str) -> Result<Point, ()> {
        let mut parts = s.split('-').skip(1);
        let x = parts.next().ok_or(())?[1..].parse().map_err(|_| ())?;
        let y = parts.next().ok_or(())?[1..].parse().map_err(|_| ())?;
        Ok(Point::new(x, y))
    }
}

impl Point {
    fn new(x: usize, y: usize) -> Point {
        Point { x, y }
    }

    fn neighbors(&self) -> Vec<Point> {
        let mut neighbors = Vec::new();
        if self.x > 0 {
            neighbors.push(Point::new(self.x - 1, self.y));
        }
        if self.y > 0 {
            neighbors.push(Point::new(self.x, self.y - 1));
        }
        neighbors.push(Point::new(self.x + 1, self.y));
        neighbors.push(Point::new(self.x, self.y + 1));
        neighbors
    }
}

struct Node {
    size: usize,
    used: usize,
    avail: usize,
}

impl Node {
    fn new_from_str(size: &str, used: &str, avail: &str) -> Node {
        Node {
            size: size.trim_end_matches('T').parse().unwrap(),
            used: used.trim_end_matches('T').parse().unwrap(),
            avail: avail.trim_end_matches('T').parse().unwrap(),
        }
    }
}

fn main() {
    let input = include_str!("../input");
    let nodes = input
        .lines()
        .skip(2)
        .map(|line| {
            let mut parts = line.split_whitespace();
            let p = parts.next().unwrap().parse().unwrap();
            let size = parts.next().unwrap();
            let used = parts.next().unwrap();
            let avail = parts.next().unwrap();
            (p, Node::new_from_str(size, used, avail))
        })
        .collect::<HashMap<Point, Node>>();

    let mut viable_pairs = 0;
    for (p1, n1) in nodes.iter() {
        for (p2, n2) in nodes.iter() {
            if p1 != p2 && n1.used > 0 && n1.used <= n2.avail {
                viable_pairs += 1;
            }
        }
    }
    println!("p1: {}", viable_pairs);

    let max_x = nodes.keys().map(|p| p.x).max().unwrap();
    let max_y = nodes.keys().map(|p| p.y).max().unwrap();
    for y in 0..=max_y {
        for x in 0..=max_x {
            let p = Point::new(x, y);
            let n = nodes.get(&p).unwrap();
            if p == Point::new(0, 0) {
                print!("G");
            } else if p == Point::new(max_x, 0) {
                print!("T");
            } else if n.used == 0 {
                print!("_");
            } else if n.used > 100 {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }

    // I used the printed grid to determine the solution to p2. LOL
}
