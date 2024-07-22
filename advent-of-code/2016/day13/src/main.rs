use std::collections::{HashMap, HashSet};

use pathfinding::directed::dijkstra;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn new(x: usize, y: usize) -> Point {
        Point { x, y }
    }

    fn neighbors(&self, favorite_number: usize) -> Vec<(Point, usize)> {
        let mut neighbors = Vec::new();
        if self.x > 0 {
            neighbors.push((Point::new(self.x - 1, self.y), 1));
        }
        if self.y > 0 {
            neighbors.push((Point::new(self.x, self.y - 1), 1));
        }
        neighbors.push((Point::new(self.x + 1, self.y), 1));
        neighbors.push((Point::new(self.x, self.y + 1), 1));
        neighbors
            .iter()
            .filter(|(p, _)| p.get_type(favorite_number) == Type::Open)
            .cloned()
            .collect()
    }

    fn get_type(&self, favorite_number: usize) -> Type {
        let mut v = self.x * self.x
            + 3 * self.x
            + 2 * self.x * self.y
            + self.y
            + self.y * self.y
            + favorite_number;
        let mut ones = 0;
        while v > 0 {
            v = v & (v - 1);
            ones += 1;
        }
        if ones % 2 == 1 {
            Type::Wall
        } else {
            Type::Open
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Wall,
    Open,
}

fn main() {
    let favorite_number = 1358;
    let start = Point::new(1, 1);
    let goal = Point::new(31, 39);

    let (_, steps) =
        dijkstra::dijkstra(&start, |p| p.neighbors(favorite_number), |p| *p == goal).unwrap();
    println!("p1: {}", steps);

    let mut frontier = vec![(start, 0)];
    let mut dist = HashMap::new();

    while let Some((point, steps)) = frontier.pop() {
        if steps > 50 {
            continue;
        }
        dist.insert(point.clone(), steps);
        for (neighbor, _) in point.neighbors(favorite_number) {
            if dist.contains_key(&neighbor) && dist[&neighbor] <= steps + 1 {
                continue;
            }
            frontier.push((neighbor, steps + 1));
        }
    }
    println!("p2: {}", dist.len());
}
