use std::{
    collections::{HashMap, HashSet},
    ops::Add,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(x: usize, y: usize) -> Point {
        Point {
            x: x as isize,
            y: y as isize,
        }
    }
}

// Impl add so we can use + operator on Point
impl Add<&Point> for &Point {
    type Output = Point;

    fn add(self, other: &Point) -> Point {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

// These are the valid direction we can move in the map.
const DIRECTIONS: [Point; 4] = [
    Point { x: 0, y: 1 },
    Point { x: 0, y: -1 },
    Point { x: 1, y: 0 },
    Point { x: -1, y: 0 },
];

struct Map {
    map: HashMap<Point, usize>,
}

impl Map {
    // Create the map from the given input string and return all the trail heads as well.
    fn new(input: &str) -> (Map, Vec<Point>) {
        // In previous days I talked about using for loops vs folds. Here is a
        // fold example to update two data structures at the same time.
        let (map, heads) = input
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                // Convert lines and chars to points.
                line.chars()
                    .filter_map(|c| c.to_digit(10))
                    .map(|c| c as usize)
                    .enumerate()
                    .map(move |(x, c)| (Point::new(x, y), c))
            })
            .fold(
                // Add the points to the map and the head if they are 0.
                (HashMap::new(), Vec::new()),
                |(mut map, mut heads), (point, c)| {
                    map.insert(point, c);
                    if c == 0 {
                        heads.push(point);
                    }
                    (map, heads)
                },
            );
        (Map { map }, heads)
    }

    // Get the value at the given point.
    fn get(&self, point: &Point) -> Option<usize> {
        self.map.get(point).cloned()
    }

    // Get the neighbors of the given point that are in the map.
    fn neighbors(&self, point: &Point) -> Vec<Point> {
        let mut neighbors = Vec::new();
        for d in DIRECTIONS.iter() {
            let neighbor = point + d;
            if let Some(_) = self.map.get(&neighbor) {
                neighbors.push(neighbor);
            }
        }
        neighbors
    }
}

fn main() {
    let input = include_str!("input.txt");
    let (map, heads) = Map::new(input);

    let now = std::time::Instant::now();
    let p1 = heads.iter().map(|head| dfs(&map, head)).sum::<usize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();
    let p2 = heads.iter().map(|head| dfs_all(&map, head)).sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
}

fn dfs(map: &Map, head: &Point) -> usize {
    // For part 1, we are only interest in the number of 9s we can reach. This
    // is a simple DFS where we can ignore nodes we've already visited.
    let mut nines = HashSet::new();
    let mut visited = HashSet::new();
    let mut stack = Vec::new();

    stack.push((*head, 0));
    while let Some((point, height)) = stack.pop() {
        if let Some(9) = map.map.get(&point) {
            nines.insert(point);
            continue;
        }
        for neighbor in map.neighbors(&point) {
            // Skip this node if we've already visited it or if it's not exactly
            // one level higher.
            if visited.contains(&neighbor) {
                continue;
            } else if map.get(&neighbor) != Some(height + 1) {
                continue;
            }
            visited.insert(neighbor);
            stack.push((neighbor, height + 1));
        }
    }

    // The solution is the number of 9s we've visited.
    nines.len()
}

fn dfs_all(map: &Map, head: &Point) -> usize {
    // For part two, we want to find all the paths. This differs from part 1 in
    // that we now want to visit all paths, so we don't need to keep track of
    // visited nodes. We can also just keep a count now because we don't care if
    // we've seen a nine before, just that the path there was unique.
    let mut paths: usize = 0;
    let mut stack = Vec::new();

    stack.push((*head, 0));
    while let Some((point, height)) = stack.pop() {
        if let Some(9) = map.map.get(&point) {
            paths += 1;
            continue;
        }
        for neighbor in map.neighbors(&point) {
            if map.get(&neighbor) != Some(height + 1) {
                continue;
            }
            stack.push((neighbor, height + 1));
        }
    }
    paths
}
