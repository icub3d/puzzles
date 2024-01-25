use colored::Colorize;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    ops::Add,
};

// We'll use a point to track where each position is on the map.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

// We impl Add here to simplify the code below.
impl Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

// These are our cardinal directions we'll use this to short-circuit
// slopes.
static UP: Point = Point::new(0, -1);
static DOWN: Point = Point::new(0, 1);
static LEFT: Point = Point::new(-1, 0);
static RIGHT: Point = Point::new(1, 0);

// In other cases, we iterate over these to find neighbors.
static DIRECTIONS: [Point; 4] = [UP, DOWN, LEFT, RIGHT];

impl Point {
    const fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    // Return all neighbors in all cardinal directions.
    fn neighbors(&self) -> Vec<Point> {
        DIRECTIONS.iter().map(|d| *self + *d).collect()
    }
}

struct Map {
    map: HashMap<Point, char>,
    start: Point,
    end: Point,
}

impl Map {
    // Parse the input into our map and find the start and end points.
    fn new(input: &str) -> Self {
        // Generate the map.
        let map = input
            .lines()
            .enumerate()
            .flat_map(|(y, l)| {
                l.chars()
                    .enumerate()
                    .map(|(x, c)| {
                        (
                            Point {
                                x: x as i32,
                                y: y as i32,
                            },
                            c,
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<HashMap<_, _>>();

        // We'll need the max x and y to find the end point. The input
        // size differs from the test size. If you are just interested
        // in one or the other, you could hard code the values.
        let max_x = map.keys().map(|p| p.x).max().unwrap();
        let max_y = map.keys().map(|p| p.y).max().unwrap();
        let start = Point { x: 1, y: 0 };
        let end = Point {
            x: max_x - 1,
            y: max_y,
        };
        Self { map, start, end }
    }

    fn neighbors_p2(&self, p: &Point) -> Vec<Point> {
        // For part 2, we don't care about and arrow indicators. Which
        // means we'll basically just return all cardinal directions
        // that are on the map and not a forest.
        let mut neighbors = Vec::new();
        for d in p.neighbors() {
            match self.map.get(&d) {
                None => continue,
                Some(c) => match c {
                    '#' => continue,
                    _ => neighbors.push(d),
                },
            }
        }
        neighbors
    }

    fn neighbors(&self, p: &Point) -> Vec<Point> {
        // For part 1, there are some rules about what makes a
        // neighbor. First of all, If we are on an arrow, we can only
        // go in that direction.
        match self.map.get(p).unwrap() {
            '>' => return vec![*p + RIGHT],
            '<' => return vec![*p + LEFT],
            '^' => return vec![*p + UP],
            'v' => return vec![*p + DOWN],
            _ => {}
        }

        // Otherwise, we'll return all cardinal directions that are on
        // the map and not a forest. I don't use the Point::neighbors
        // because we want to know the delta to determine if we can go
        // into a slope.
        let mut neighbors = Vec::new();
        for d in DIRECTIONS.iter() {
            let p = *p + *d;
            match self.map.get(&p) {
                None => continue,
                Some(c) => match (c, d.x, d.y) {
                    // We can't go back up a slope, so I wrote it sort
                    // of the opposite as one normally would. The 5
                    // conditions below are the invalid neighbors and
                    // then all others would be valid.
                    ('#', _, _) => continue,
                    ('>', -1, 0) => continue,
                    ('<', 1, 0) => continue,
                    ('^', 0, 1) => continue,
                    ('v', 0, -1) => continue,
                    _ => neighbors.push(p),
                },
            }
        }
        neighbors
    }

    fn longest_path_frontier(&self) -> usize {
        // When we reach the end, we'll want to keep track of
        // it. We'll return the largest of the values.
        let mut ends = Vec::new();

        // Track paths that we've seen.
        let seen = HashSet::new();

        // Track the "work" to do.
        let mut frontier = VecDeque::new();
        frontier.push_back((self.start, seen.clone(), 0));

        // Pop something of the frontier and see what we can do with it.
        while let Some((p, mut seen, steps)) = frontier.pop_front() {
            // If we've reached the end, we'll want to keep track of
            // it and we are done with this path.
            if p == self.end {
                ends.push(steps);
                continue;
            }

            // If we've already seen this point, we don't need to continue.
            if seen.contains(&p) {
                continue;
            }

            // Add myself to the seen set and add all my neighbors to
            // the frontier. Note that I'm adding seen to my
            // frontier. When we branch, our seen set will diverge (i
            // think?).
            seen.insert(p);
            for n in self.neighbors(&p).iter().filter(|n| !seen.contains(n)) {
                frontier.push_back((*n, seen.clone(), steps + 1));
            }
        }
        *ends.iter().max().unwrap()
    }

    fn dfs(&self, p: Point, seen: &mut HashSet<Point>, steps: usize, ends: &mut Vec<usize>) {
        // If we reach the end, we'll want to keep track of it.
        if p == self.end {
            ends.push(steps);
            return;
        }

        // If we've already seen this point, we don't need to continue.
        if seen.contains(&p) {
            return;
        }

        // Add myself to the seen set and go down all my neighbors.
        seen.insert(p);
        for n in self.neighbors(&p) {
            if seen.contains(&n) {
                continue;
            }
            self.dfs(n, seen, steps + 1, ends);
        }
        // remove myself for the other branches.
        seen.remove(&p);
    }

    fn longest_path_dfs(&self) -> usize {
        let mut ends = Vec::new();
        let mut seen = HashSet::new();
        self.dfs(self.start, &mut seen, 0, &mut ends);
        *ends.iter().max().unwrap()
    }

    fn print(&self) {
        let max_x = self.map.keys().map(|p| p.x).max().unwrap();
        let max_y = self.map.keys().map(|p| p.y).max().unwrap();
        for y in 0..=max_y {
            for x in 0..=max_x {
                let p = Point { x, y };
                match self.map.get(&p).unwrap() {
                    '#' => print!("#"),
                    '>' => print!("{}", ">".green()),
                    '<' => print!("{}", "<".green()),
                    '^' => print!("{}", "^".green()),
                    'v' => print!("{}", "v".green()),
                    _ => {
                        let n = self.neighbors_p2(&p).len();
                        if n <= 2 {
                            print!(".");
                        } else {
                            match n {
                                4 => print!("{}", "4".red()),
                                3 => print!("{}", "3".yellow()),
                                _ => unreachable!(),
                            }
                        }
                    }
                }
            }
            println!();
        }
    }

    fn find_branching_edges(&self) -> HashMap<Point, Vec<(Point, usize)>> {
        // Relabel the map with the count of neighbors.
        let map = self
            .map
            .iter()
            .filter(|(_, c)| **c != '#')
            .map(|(p, _)| {
                let n = self.neighbors_p2(p).len();
                (*p, n)
            })
            .collect::<HashMap<_, _>>();

        // Find our start, end, and branching nodes (neighbors >
        // 2). They will be our nodes in our simplified graph.
        let nodes = map
            .iter()
            .filter(|(_, n)| **n != 2)
            .map(|(p, _)| *p)
            .collect::<HashSet<_>>();

        // Find all the edges between nodes. We'll basically walk in
        // each valid direction and stop when we hit another node from
        // above.
        let mut edges: HashMap<Point, Vec<(Point, usize)>> = HashMap::new();
        for node in nodes.iter() {
            for mut neighbor in self.neighbors_p2(node) {
                // Track our previous node.
                let mut prev = *node;

                // Track the distance we've traveled to get to this branch.
                let mut dist = 0;

                // Walk until we hit another node.
                loop {
                    dist += 1;

                    // Find the neighbors of this neighbor.
                    let neighbors = self.neighbors_p2(&neighbor);
                    let neighbors = neighbors.iter().filter(|n| **n != prev).collect::<Vec<_>>();

                    // If it's more than one, we've hit a branch.
                    if neighbors.len() != 1 {
                        edges.entry(*node).or_default().push((neighbor, dist));
                        break;
                    }

                    // Otherwise, we'll continue walking.
                    prev = neighbor;
                    neighbor = *neighbors[0];
                }
            }
        }

        edges
    }

    fn longest_path_branches_frontier(&self) -> usize {
        // Find the reduced graph.
        let edges = self.find_branching_edges();
        println!("nodes: {:?}", edges.len());
        println!(
            "edges: {:?}",
            edges.values().map(|e| e.len()).sum::<usize>()
        );

        // Now we can do the longest path again, but with our reduced
        // graph. I won't comment this one again because it's
        // basically the same as the naive approach.
        let mut ends = Vec::new();
        let seen = HashSet::new();
        let mut frontier = VecDeque::new();
        frontier.push_back((self.start, seen.clone(), 0));
        while let Some((p, mut seen, steps)) = frontier.pop_front() {
            if p == self.end {
                ends.push(steps);
                continue;
            }
            if seen.contains(&p) {
                continue;
            }
            seen.insert(p);
            for (n, c) in edges
                .get(&p)
                .unwrap()
                .iter()
                .filter(|n| !seen.contains(&n.0))
            {
                frontier.push_back((*n, seen.clone(), steps + c));
            }
        }
        *ends.iter().max().unwrap()
    }

    fn longest_path_branches_dfs(&self) -> usize {
        // Find the reduced graph.
        let edges = self.find_branching_edges();

        // Use dfs to find the longest path.
        let mut seen = HashSet::new();
        let mut ends = Vec::new();
        self.dfs_p2(&edges, self.start, &mut seen, 0, &mut ends);
        *ends.iter().max().unwrap()
    }

    fn dfs_p2(
        &self,
        edges: &HashMap<Point, Vec<(Point, usize)>>,
        p: Point,
        seen: &mut HashSet<Point>,
        steps: usize,
        ends: &mut Vec<usize>,
    ) {
        // This is all similar to dfs for part one, but we are using
        // the edges we gathered from the branches instead.
        if p == self.end {
            ends.push(steps);
            return;
        }
        if seen.contains(&p) {
            return;
        }

        // Add myself to the seen set and go down all my neighbors.
        seen.insert(p);
        let neighbors = edges.get(&p).unwrap();
        for (neighbor, cost) in neighbors {
            if seen.contains(neighbor) {
                continue;
            }
            self.dfs_p2(edges, *neighbor, seen, steps + cost, ends);
        }
        // remove myself for the other branches.
        seen.remove(&p);
    }
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let map = Map::new(&input);

    // Print the grid.
    map.print();

    // Calculate the branches for part 1.
    let branches = map
        .map
        .iter()
        .filter(|(_, c)| **c != '#')
        .map(|(p, _)| map.neighbors(p).len())
        .filter(|n| *n > 2)
        .count();
    println!("p1-branches: {:?}", branches);

    // Part 1 - naive approach, it works because the branching is
    // smaller here.
    let now = std::time::Instant::now();
    let p1 = map.longest_path_frontier();
    println!("p1: {:?} ({:?})", p1, now.elapsed());

    // Do the same but with dfs.
    let now = std::time::Instant::now();
    let p1 = map.longest_path_dfs();
    println!("p1: {:?} ({:?})", p1, now.elapsed());

    // Calculate the branches for part 2.
    let branches = map
        .map
        .iter()
        .filter(|(_, c)| **c != '#')
        .map(|(p, _)| map.neighbors_p2(p).len())
        .filter(|n| *n > 2)
        .count();
    println!("p2-branches: {:?}", branches);

    // Part 2 - look for branches in the path and then create a graph
    // to and from them. This will simplify the graph considerably,
    // making the search much faster.
    let now = std::time::Instant::now();
    let p2 = map.longest_path_branches_frontier();
    println!("p2: {:?} ({:?})", p2, now.elapsed());

    // Use dfs instead.
    let now = std::time::Instant::now();
    let p2 = map.longest_path_branches_dfs();
    println!("p2: {:?} ({:?})", p2, now.elapsed());
}
