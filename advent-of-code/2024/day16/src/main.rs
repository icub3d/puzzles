use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap, HashSet},
    hash::Hash,
};

use itertools::Itertools;
use strum::{EnumIter, IntoEnumIterator};

// Track our state in the priority queue. We only are interested sorting by
// cost. The rest of the information is simply used for
// tracking and finding neighbors.
#[derive(Eq, PartialEq, Hash)]
struct State {
    point: Point,
    direction: Direction,
    cost: usize,
    path: Vec<Point>,
}

impl State {
    fn new(point: Point, direction: Direction, cost: usize, path: Vec<Point>) -> Self {
        Self {
            point,
            direction,
            path,
            cost,
        }
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// The directions the robot can move. We use strum's EnumIter to iterate over
// all the variants.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, EnumIter)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    // Given a point, return the new point after moving in the direction.
    fn go(&self, point: &Point) -> Point {
        match self {
            Direction::North => Point::new(point.x, point.y - 1),
            Direction::South => Point::new(point.x, point.y + 1),
            Direction::West => Point::new(point.x - 1, point.y),
            Direction::East => Point::new(point.x + 1, point.y),
        }
    }

    // Return the opposite direction.
    fn opposite(&self) -> Self {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::West => Direction::East,
            Direction::East => Direction::West,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    // Find all the neighbors of a point in all directions except the opposite.
    // The cost is 1 if the same direction is maintained, 1001 otherwise.
    fn neighbors(&self, direction: Direction) -> Vec<(Point, Direction, usize)> {
        Direction::iter()
            .filter(|d| *d != direction.opposite())
            .map(|d| {
                let cost = if direction != d { 1001 } else { 1 };
                (d.go(self), d, cost)
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
struct Map {
    nodes: HashSet<Point>,
    start: Point,
    end: Point,
}

impl Map {
    fn new(input: &str) -> Self {
        let mut nodes = HashSet::new();
        let mut start = Point { x: 0, y: 0 };
        let mut end = Point { x: 0, y: 0 };

        // Add all the nodes and find the start and end points.
        for (y, line) in input.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                if c == 'S' {
                    start = Point { x, y };
                } else if c == 'E' {
                    end = Point { x, y };
                }
                if c != '#' {
                    nodes.insert(Point { x, y });
                }
            }
        }

        Self { nodes, start, end }
    }

    // Find all the neighbors of a point and the cost to reach them.
    fn neighbors(&self, point: Point, direction: Direction) -> Vec<(Point, Direction, usize)> {
        // We can just use the Points neighbor function but filter out nodes not
        // in the map.
        point
            .neighbors(direction)
            .into_iter()
            .filter(|(p, _, _)| self.nodes.contains(p))
            .collect()
    }

    // Return the length of the shortest path and all the paths that could get
    // there in that distance.
    fn shortest_paths(&self) -> (usize, Vec<Vec<Point>>) {
        // Track all our paths and best cost.
        let mut paths = Vec::new();
        let mut best = std::usize::MAX;

        // Track all nodes we've seen and the cost to get there. We can use
        // this to avoid revisiting nodes with a higher cost.
        let mut visited: HashMap<(Point, Direction), usize> = HashMap::new();

        // Use a priority queue to explore the nodes with the lowest cost first.
        // This will help us find the shortest path and avoid exploring paths
        // that are already more expensive than the best path.
        let mut frontier = BinaryHeap::new();
        frontier.push(State::new(self.start, Direction::East, 0, vec![self.start]));

        // Loop through the frontier.
        while let Some(State {
            point,
            direction,
            path,
            cost,
        }) = frontier.pop()
        {
            // If we have already visited this node with a lower cost, skip it.
            // Otherwise, add it to the visited nodes.
            if let Some(&prev_cost) = visited.get(&(point, direction)) {
                if cost > prev_cost {
                    continue;
                }
            } else {
                visited.insert((point, direction), cost);
            }

            // If we've reached the end node, because we are using a priority queue,
            // we'll have found one shortest path. We can add it to the paths and
            // update the best cost.
            if point == self.end && cost <= best {
                paths.push(path.clone());
                best = cost;
            }

            for (neighbor, new_direction, neighbor_cost) in self.neighbors(point, direction) {
                frontier.push(State::new(neighbor, new_direction, cost + neighbor_cost, {
                    let mut path = path.clone();
                    path.push(neighbor);
                    path
                }));
            }
        }

        (best, paths)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");

    let now = std::time::Instant::now();
    let map = Map::new(input);
    let (p1, paths) = map.shortest_paths();
    println!("p1: {:?}", p1);

    let p2 = paths.iter().flatten().unique().count();
    println!("p2: {:?}", p2);

    println!("Time: {:?}", now.elapsed());
    Ok(())
}
