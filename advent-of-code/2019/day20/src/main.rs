use std::{
    collections::{HashMap, HashSet},
    ops::{Add, Sub},
};

use pathfinding::directed::dijkstra::dijkstra;

const INPUT: &str = include_str!("../input");

const DELTAS: [Point; 4] = [
    Point { x: 0, y: -1 },
    Point { x: 1, y: 0 },
    Point { x: 0, y: 1 },
    Point { x: -1, y: 0 },
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl Sub for Point {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Point {
    fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    fn neighbors(&self) -> Vec<Self> {
        vec![
            *self + DELTAS[0],
            *self + DELTAS[1],
            *self + DELTAS[2],
            *self + DELTAS[3],
        ]
    }
}

fn main() {
    let grid = INPUT
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(move |(x, c)| (Point::new(x as isize, y as isize), c))
        })
        .collect::<HashMap<_, _>>();

    // Find all the portals.
    let mut portals: HashMap<String, HashSet<Point>> = HashMap::new();
    for (&point, &c) in &grid {
        if c.is_ascii_uppercase() {
            for delta in DELTAS {
                let neighbor = point + delta;
                if let Some(&next) = grid.get(&neighbor) {
                    if next.is_ascii_uppercase() {
                        // Get the name and position.
                        let name = if delta == DELTAS[0] || delta == DELTAS[3] {
                            format!("{}{}", next, c)
                        } else {
                            format!("{}{}", c, next)
                        };
                        let position = neighbor + delta;
                        // If the next character is a dot, add the portal to the list.
                        if let Some(&next) = grid.get(&position) {
                            if next == '.' {
                                // Add the portal to the list.
                                portals.entry(name).or_default().insert(position);
                            }
                        } else {
                            // If the next character is a space, add the portal to the list.
                            let position = point - delta;
                            if let Some(&next) = grid.get(&position) {
                                if next == '.' {
                                    // Add the portal to the list.
                                    portals.entry(name).or_default().insert(position);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    let min_x = 2;
    let min_y = 2;
    let max_x = grid.keys().map(|p| p.x).max().unwrap() - 2;
    let max_y = grid.keys().map(|p| p.y).max().unwrap() - 2;

    let outer = portals
        .values()
        .flatten()
        .filter(|&&p| p.x == min_x || p.x == max_x || p.y == min_y || p.y == max_y)
        .cloned()
        .collect::<HashSet<_>>();

    // Find the start and end positions.
    let aa = *portals.get("AA").unwrap().iter().next().unwrap();
    let zz = *portals.get("ZZ").unwrap().iter().next().unwrap();

    // Turn the portals into edges.
    let portals = portals
        .into_iter()
        .filter_map(|(name, positions)| {
            if name == "AA" || name == "ZZ" {
                return None;
            }
            let positions = positions.into_iter().collect::<Vec<_>>();
            Some(vec![
                (positions[0], positions[1]),
                (positions[1], positions[0]),
            ])
        })
        .flatten()
        .collect::<HashMap<_, _>>();

    let neighbors = |point: &Point| -> Vec<(Point, usize)> {
        let mut neighbors: Vec<_> = point
            .neighbors()
            .into_iter()
            .map(|p| (p, 1))
            .filter(|(point, _)| {
                if let Some(&c) = grid.get(point) {
                    c == '.'
                } else {
                    false
                }
            })
            .collect();
        if let Some(&next) = portals.get(point) {
            neighbors.push((next, 1));
        }
        neighbors
    };

    let p1 = dijkstra(&aa, neighbors, |point| *point == zz).unwrap().1;
    println!("p1: {}", p1);

    let start = State {
        point: aa,
        level: 0,
    };
    let end = State {
        point: zz,
        level: 0,
    };
    let neighbors = |state: &State| -> Vec<(State, usize)> {
        let mut neighbors: Vec<_> = state
            .point
            .neighbors()
            .into_iter()
            .map(|p| {
                (
                    State {
                        point: p,
                        level: state.level,
                    },
                    1,
                )
            })
            .filter(|(state, _)| {
                if let Some(&c) = grid.get(&state.point) {
                    c == '.'
                } else {
                    false
                }
            })
            .collect();
        if let Some(&next) = portals.get(&state.point) {
            if (state.level == 0 && outer.contains(&state.point))
                || (state.level > 0 && (state.point == aa || state.point == zz))
            {
            } else if outer.contains(&state.point) {
                neighbors.push((
                    State {
                        point: next,
                        level: state.level - 1,
                    },
                    1,
                ));
            } else {
                neighbors.push((
                    State {
                        point: next,
                        level: state.level + 1,
                    },
                    1,
                ));
            }
        }
        neighbors
    };
    let p2 = dijkstra(&start, neighbors, |&state| state == end)
        .unwrap()
        .1;
    println!("p2: {}", p2);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    point: Point,
    level: usize,
}
