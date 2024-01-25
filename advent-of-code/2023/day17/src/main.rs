use std::collections::{BinaryHeap, HashMap};

// We need to track the direction we're coming from to make sure we
// don't go further in one direction than we're allowed.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    // Return the opposite direction.
    fn opposite(&self) -> Self {
        match self {
            Self::North => Self::South,
            Self::South => Self::North,
            Self::East => Self::West,
            Self::West => Self::East,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    // Return the valid next points from this point. It won't include
    // any that are out of bounds.
    fn valid_next(&self, grid: &[Vec<usize>]) -> Vec<(Direction, Point)> {
        let mut next = Vec::new();
        if self.x > 0 {
            next.push((Direction::West, Self::new(self.x - 1, self.y)));
        }
        if self.y > 0 {
            next.push((Direction::North, Self::new(self.x, self.y - 1)));
        }
        if self.x < grid[0].len() - 1 {
            next.push((Direction::East, Self::new(self.x + 1, self.y)));
        }
        if self.y < grid.len() - 1 {
            next.push((Direction::South, Self::new(self.x, self.y + 1)));
        }
        next
    }
}

// We'll use this to track our current position and direction and how
// many times we've gone that direction.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Node {
    position: Point,
    direction: Direction,
    direction_count: usize,
}

impl Node {
    fn new(position: Point, direction: Direction, direction_count: usize) -> Self {
        Self {
            position,
            direction,
            direction_count,
        }
    }
}

fn neighbors_combined<const MIN: usize, const MAX: usize>(
    node: &Node,
    grid: &[Vec<usize>],
) -> Vec<Node> {
    let mut neighbors = Vec::new();
    // Get the possible next valid points.
    for (d, p) in node.position.valid_next(grid) {
        if d == node.direction.opposite() {
            // We can't go in the opposite direction.
            continue;
        } else if d != node.direction && node.direction_count >= MIN {
            // If we are going in a new direction, makes sure we've
            // gone at least min times.
            neighbors.push(Node::new(p, d, 1));
        } else if d == node.direction && node.direction_count < MAX {
            // If we in the same direction, make sure it's less than max.
            neighbors.push(Node::new(p, d, node.direction_count + 1));
        }

        // All other cases are invalid for p1.
    }
    neighbors
}

#[allow(dead_code)]
fn neighbors(node: &Node, grid: &[Vec<usize>]) -> Vec<Node> {
    let mut neighbors = Vec::new();
    // Get the possible next valid points.
    for (d, p) in node.position.valid_next(grid) {
        if d == node.direction.opposite() {
            // We can't go in the opposite direction.
            continue;
        } else if d != node.direction {
            // If we are going in a new direction, then we can use it,
            // just reset the count.
            neighbors.push(Node::new(p, d, 1));
        } else if node.direction_count < 3 {
            // If we have been going in this direction for less than
            // 3, we can keep going.
            neighbors.push(Node::new(p, d, node.direction_count + 1));
        }

        // All other cases are invalid for p1.
    }
    neighbors
}

#[allow(dead_code)]
fn neighbors_p2(node: &Node, grid: &[Vec<usize>]) -> Vec<Node> {
    let mut neighbors = Vec::new();
    for (d, p) in node.position.valid_next(grid) {
        if d == node.direction.opposite() {
            // We can't go in the opposite direction.
            continue;
        } else if d != node.direction && node.direction_count >= 4 {
            // We can only change direction if we've already gone in
            // this direction 4 times or more.
            neighbors.push(Node::new(p, d, 1));
        } else if d == node.direction && node.direction_count < 10 {
            // We can only go in the same direction if we haven't gone
            // more than 10 times in this durection.
            neighbors.push(Node::new(p, d, node.direction_count + 1));
        }

        // All other cases are invalid for p2.
    }
    neighbors
}

// The state we'll track in our priority queue. We need to track the
// node above and the cost to get there.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    cost: usize,
    node: Node,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // We are using a min heap, so we are doing this backwards.
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.node.cmp(&other.node))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn dijkstra<F, G>(grid: &[Vec<usize>], start: &Point, goal_fn: G, neighbor_fn: F) -> Option<usize>
where
    F: Fn(&Node, &[Vec<usize>]) -> Vec<Node>,
    G: Fn(&Node) -> bool,
{
    // Track our min distances at each Node. In our specific case, we
    // have multiple because we could be coming from South or East
    // at the start.
    let mut distances = HashMap::new();
    distances.insert(Node::new(start.clone(), Direction::South, 0), 0);
    distances.insert(Node::new(start.clone(), Direction::East, 0), 0);

    // Track paths we want to visit. Again, we are adding two because
    // we could be coming from either.
    let mut frontier = BinaryHeap::new();
    frontier.push(State {
        cost: 0,
        node: Node::new(start.clone(), Direction::South, 0),
    });
    frontier.push(State {
        cost: 0,
        node: Node::new(start.clone(), Direction::East, 0),
    });

    // Grab the next node from the frontier.
    while let Some(State { cost, node }) = frontier.pop() {
        // If we are at the goal, we are done.
        if goal_fn(&node) {
            return Some(cost);
        }

        // Otherwise, check our neighbors.
        for neighbor in neighbor_fn(&node, grid) {
            // If we've already visited this node and it was cheaper,
            // we don't need to keep checking this way.
            let new_cost = cost + grid[neighbor.position.y][neighbor.position.x];
            if let Some(&best) = distances.get(&neighbor) {
                if new_cost >= best {
                    continue;
                }
            }

            // Otherwise, add it to our distances and frontier.
            distances.insert(neighbor.clone(), new_cost);
            frontier.push(State {
                cost: new_cost,
                node: neighbor,
            });
        }
    }

    // If we get here, we didn't find a path. Not possible in our
    // case, but is in the general case.
    None
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let grid = input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    // Part 1
    let start = Point::new(0, 0);
    let goal = Point::new(grid[0].len() - 1, grid.len() - 1);
    let now = std::time::Instant::now();
    println!(
        "p1: {:?} ({:?})",
        dijkstra(
            &grid,
            &start,
            |n| n.position == goal,
            neighbors_combined::<1, 3>
        ),
        now.elapsed()
    );

    // Part 2 is the same as part 1, but with different neighbors.
    let now = std::time::Instant::now();
    println!(
        "p2: {:?} ({:?})",
        dijkstra(
            &grid,
            &start,
            |n| n.position == goal && n.direction_count >= 4,
            neighbors_combined::<4, 10>,
        ),
        now.elapsed()
    );

    // Is it OK to reuse algos from other packages?
    let now = std::time::Instant::now();
    let n = Node::new(start.clone(), Direction::South, 0);
    let p1 = pathfinding::directed::dijkstra::dijkstra(
        &n,
        |n| {
            neighbors_combined::<1, 3>(n, &grid)
                .into_iter()
                .map(|n| (n.clone(), grid[n.position.y][n.position.x]))
        },
        |n| n.position == goal,
    );
    println!("p1-pathfinding: {:?} ({:?}))", p1.unwrap().1, now.elapsed());

    let now = std::time::Instant::now();
    let n = Node::new(start.clone(), Direction::East, 0);
    let p2 = pathfinding::directed::dijkstra::dijkstra(
        &n,
        |n| {
            neighbors_combined::<4, 10>(n, &grid)
                .into_iter()
                .map(|n| (n.clone(), grid[n.position.y][n.position.x]))
        },
        |n| n.position == goal && n.direction_count >= 4,
    );
    println!("p2-pathfinding: {:?} ({:?}))", p2.unwrap().1, now.elapsed());
}
