use std::collections::VecDeque;

use md5::{Digest, Md5};
use pathfinding::prelude::dijkstra;

fn md5(password: &str, prev: &[char]) -> [char; 4] {
    let mut hasher = Md5::new();
    hasher.update(password.as_bytes());
    hasher.update(prev.iter().collect::<String>().as_bytes());
    let hash = format!("{:x}", hasher.finalize())
        .chars()
        .collect::<Vec<_>>();
    [hash[0], hash[1], hash[2], hash[3]]
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn new(x: usize, y: usize) -> Point {
        Point { x, y }
    }

    fn neighbors(&self, open: &[char; 4]) -> Vec<(Point, char)> {
        let mut neighbors = Vec::new();
        if self.x > 0 && "bcdef".contains(open[2]) {
            neighbors.push((Point::new(self.x - 1, self.y), 'L'));
        }
        if self.x < 3 && "bcdef".contains(open[3]) {
            neighbors.push((Point::new(self.x + 1, self.y), 'R'));
        }
        if self.y > 0 && "bcdef".contains(open[0]) {
            neighbors.push((Point::new(self.x, self.y - 1), 'U'));
        }
        if self.y < 3 && "bcdef".contains(open[1]) {
            neighbors.push((Point::new(self.x, self.y + 1), 'D'));
        }
        neighbors
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct State {
    point: Point,
    prev: Vec<char>,
}

impl State {
    fn new(point: Point, prev: Vec<char>) -> State {
        State { point, prev }
    }

    fn neighbors(&self, password: &str) -> Vec<(State, usize)> {
        let mut neighbors = Vec::new();
        let hash = md5(password, &self.prev);
        for (p, c) in self.point.neighbors(&hash) {
            neighbors.push((
                State::new(p, {
                    let mut prev = self.prev.clone();
                    prev.push(c);
                    prev
                }),
                1,
            ));
        }
        neighbors
    }
}

fn main() {
    let input = "ioramepc";
    // let input = "ulqzkmiv";
    let start = State::new(Point::new(0, 0), Vec::new());
    let end = |state: &State| state.point.x == 3 && state.point.y == 3;
    let neighbors = |state: &State| state.neighbors(input);

    let (path, _) = dijkstra(&start, neighbors, end).unwrap();
    println!(
        "p1: {}",
        path.last().unwrap().prev.iter().collect::<String>()
    );

    let mut frontier = VecDeque::new();
    frontier.push_back(State::new(Point::new(0, 0), Vec::new()));

    let mut max = 0;
    while let Some(state) = frontier.pop_front() {
        if state.point.x == 3 && state.point.y == 3 {
            max = max.max(state.prev.len());
            continue;
        }
        for (neighbor, _) in state.neighbors(input) {
            frontier.push_back(neighbor);
        }
    }
    println!("p2: {}", max);
}
