use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    ops::Add,
};

use pathfinding::directed::dijkstra::dijkstra;

const INPUT: &str = include_str!("../input");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Point {
    x: usize,
    y: usize,
}

impl Add for Point {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Point {
    fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    fn neighbors(&self) -> Vec<Self> {
        vec![
            Point::new(self.x + 1, self.y),
            Point::new(self.x - 1, self.y),
            Point::new(self.x, self.y + 1),
            Point::new(self.x, self.y - 1),
        ]
    }
}

fn main() {
    let grid = INPUT
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars()
                .enumerate()
                .map(move |(x, c)| (Point::new(x, y), c))
        })
        .collect::<HashMap<_, _>>();

    // Determine edges between points of interest as they'll be the only interesting neighbors in
    // our pathfinding algorithm.
    let mut edges = HashMap::new();
    for p in grid
        .iter()
        .filter(|(_, c)| **c != '.' && **c != '#')
        .map(|(p, _)| *p)
    {
        edges.insert(p, bfs(&grid, &p));
    }

    // We want to track the number of keys we need to collect and where we start.
    let keys = grid.values().filter(|&&c| c.is_ascii_lowercase()).count();
    let start = grid
        .iter()
        .find(|(_, c)| **c == '@')
        .map(|(p, _)| *p)
        .unwrap();

    let neighbors = |s: &State| {
        let mut neighbors = vec![];
        for (p, &d) in edges.get(&s.position).unwrap() {
            let c = grid.get(p).unwrap();
            match c {
                '@' => {
                    neighbors.push((
                        State {
                            position: *p,
                            keys: s.keys.clone(),
                        },
                        d,
                    ));
                }
                c if c.is_ascii_lowercase() => {
                    let mut keys = s.keys.clone();
                    keys.insert(*c);
                    neighbors.push((State { position: *p, keys }, d));
                }
                c if c.is_ascii_uppercase() => {
                    if s.keys.contains(&c.to_ascii_lowercase()) {
                        neighbors.push((
                            State {
                                position: *p,
                                keys: s.keys.clone(),
                            },
                            d,
                        ));
                    }
                }
                _ => {
                    panic!("unexpected character: {}", c)
                }
            }
        }
        neighbors
    };
    let p1 = dijkstra(
        &State {
            position: start,
            keys: BTreeSet::new(),
        },
        neighbors,
        |s| s.keys.len() == keys,
    )
    .unwrap()
    .1;
    println!("p1: {}", p1);

    let mut grid = INPUT
        .lines()
        .enumerate()
        .flat_map(|(y, l)| {
            l.chars()
                .enumerate()
                .map(move |(x, c)| (Point::new(x, y), c))
        })
        .collect::<HashMap<_, _>>();
    let start = grid
        .iter()
        .find(|(_, c)| **c == '@')
        .map(|(p, _)| *p)
        .unwrap();
    grid.insert(start, '#');
    grid.insert(Point::new(start.x + 1, start.y), '#');
    grid.insert(Point::new(start.x - 1, start.y), '#');
    grid.insert(Point::new(start.x, start.y + 1), '#');
    grid.insert(Point::new(start.x, start.y - 1), '#');
    grid.insert(Point::new(start.x + 1, start.y + 1), '@');
    grid.insert(Point::new(start.x - 1, start.y + 1), '@');
    grid.insert(Point::new(start.x + 1, start.y - 1), '@');
    grid.insert(Point::new(start.x - 1, start.y - 1), '@');

    let mut edges = HashMap::new();
    for p in grid
        .iter()
        .filter(|(_, c)| **c != '.' && **c != '#')
        .map(|(p, _)| *p)
    {
        edges.insert(p, bfs(&grid, &p));
    }

    let start = State2 {
        positions: [
            Point::new(start.x + 1, start.y + 1),
            Point::new(start.x - 1, start.y + 1),
            Point::new(start.x + 1, start.y - 1),
            Point::new(start.x - 1, start.y - 1),
        ],
        keys: BTreeSet::new(),
    };

    let neighbors = |s: &State2| {
        let mut neighbors = vec![];
        for (i, p) in s.positions.iter().enumerate() {
            for (p, &d) in edges.get(p).unwrap() {
                let c = grid.get(p).unwrap();
                match c {
                    '@' => {
                        let mut positions = s.positions;
                        positions[i] = *p;
                        neighbors.push((
                            State2 {
                                positions,
                                keys: s.keys.clone(),
                            },
                            d,
                        ));
                    }
                    c if c.is_ascii_lowercase() => {
                        let mut keys = s.keys.clone();
                        keys.insert(*c);
                        let mut positions = s.positions;
                        positions[i] = *p;
                        neighbors.push((State2 { positions, keys }, d));
                    }
                    c if c.is_ascii_uppercase() => {
                        if s.keys.contains(&c.to_ascii_lowercase()) {
                            let mut positions = s.positions;
                            positions[i] = *p;
                            neighbors.push((
                                State2 {
                                    positions,
                                    keys: s.keys.clone(),
                                },
                                d,
                            ));
                        }
                    }
                    _ => {
                        panic!("unexpected character: {}", c)
                    }
                }
            }
        }
        neighbors
    };

    let p2 = dijkstra(&start, neighbors, |s| s.keys.len() == keys)
        .unwrap()
        .1;
    println!("p2: {}", p2);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State2 {
    positions: [Point; 4],
    keys: BTreeSet<char>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    position: Point,
    keys: BTreeSet<char>,
}

fn bfs(grid: &HashMap<Point, char>, start: &Point) -> HashMap<Point, usize> {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    queue.push_back((*start, 0));
    visited.insert(*start);
    let mut neighbors = HashMap::new();

    while let Some((p, d)) = queue.pop_front() {
        if let Some(c) = grid.get(&p) {
            if *c != '.' && p != *start {
                neighbors.insert(p, d);
                continue;
            }
        }
        for n in p.neighbors() {
            if visited.contains(&n) {
                continue;
            }
            if let Some(c) = grid.get(&n) {
                if *c == '#' {
                    continue;
                }
                visited.insert(n);
                queue.push_back((n, d + 1));
            }
        }
    }
    neighbors
}
