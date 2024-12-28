use std::collections::{BinaryHeap, HashMap, HashSet};
use std::hash::Hash;

use nom::{
    bytes::complete::tag, character::complete::digit1, combinator::map_res, multi::separated_list1,
    sequence::separated_pair, IResult,
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: isize,
    y: isize,
}

impl Point {
    fn new(x: isize, y: isize) -> Self {
        Point { x, y }
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (x, y)) = separated_pair(
            map_res(digit1, |s: &str| s.parse()),
            tag(","),
            map_res(digit1, |s: &str| s.parse()),
        )(input)?;
        Ok((input, Point::new(x, y)))
    }

    // Find all the neighbors of the given point that satisfy the filter.
    fn neighbors<F>(&self, filter: F) -> Vec<Point>
    where
        F: Fn(&Point) -> bool,
    {
        [(-1, 0), (1, 0), (0, -1), (0, 1)]
            .iter()
            .map(|(x, y)| Point {
                x: self.x + x,
                y: self.y + y,
            })
            .filter(filter)
            .collect()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");
    const LEN_X: isize = 71;
    const LEN_Y: isize = 71;
    const INITIAL_BYTES: usize = 1024;

    // Parse our corrupted points.
    let (_, corruptions) = separated_list1(tag("\r\n"), Point::parse)(input)?;

    // Our start and end point.
    let start = Point::new(0, 0);
    let end = Point::new(LEN_X - 1, LEN_Y - 1);

    // We are successful if we reach the end point.
    let success = |p: &Point| *p == end;

    // Our successors are the neighbors of the current point that are within bounds and not corrupted.
    let corrupted = corruptions[..INITIAL_BYTES]
        .iter()
        .cloned()
        .collect::<HashSet<Point>>();
    let successors = |p: &Point| {
        p.neighbors(|p: &Point| {
            p.x >= 0 && p.x < LEN_X && p.y >= 0 && p.y < LEN_Y && !corrupted.contains(p)
        })
        .into_iter()
        .map(|p| (p, 1))
        .collect::<Vec<(Point, usize)>>()
    };

    // Our heuristic is the Manhattan distance to the end point.
    let heuristic = |p: &Point| ((end.x - p.x).abs() + (end.y - p.y).abs()) as usize;

    // Run A* and Dijkstra's algorithm to compare the number of visits.
    let now = std::time::Instant::now();
    match dijkstra(&start, successors, success, heuristic) {
        Some((_, steps, visited)) => {
            println!(
                "p1-a-star: {} (visited: {}, {:?})",
                steps,
                visited,
                now.elapsed()
            );
        }
        None => {
            println!("p1-a-star: No path found");
        }
    }
    let now = std::time::Instant::now();
    match dijkstra(&start, successors, success, |_: &Point| 0) {
        Some((_, steps, visited)) => {
            println!(
                "p1-a-dijkstra: {} (visited: {}, {:?})",
                steps,
                visited,
                now.elapsed()
            );
        }
        None => {
            println!("p1-a-dijkstra: No path found");
        }
    }

    // Find the first corruption that prevents us from reaching the end point.
    let now = std::time::Instant::now();
    let first = (INITIAL_BYTES..corruptions.len())
        .into_par_iter()
        .find_map_first(|i| {
            let corrupted = corruptions[..i].iter().cloned().collect::<HashSet<Point>>();
            let successors = |p: &Point| {
                p.neighbors(|p: &Point| {
                    p.x >= 0 && p.x < LEN_X && p.y >= 0 && p.y < LEN_Y && !corrupted.contains(p)
                })
                .into_iter()
                .map(|p| (p, 1))
                .collect::<Vec<(Point, usize)>>()
            };

            match dijkstra(&start, successors, success, heuristic) {
                Some(_) => None,
                None => Some(i),
            }
        });

    if let Some(i) = first {
        // Off by one, LOL.
        let corruption = corruptions[i - 1];
        println!("p2: {},{} {:?}", corruption.x, corruption.y, now.elapsed());
    } else {
        println!("p2: No path found");
    }

    Ok(())
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct State<T>
where
    T: Eq + Hash + Clone,
{
    node: T,
    cost: usize,
    heuristic: usize,
}

impl<T> PartialOrd for State<T>
where
    T: Eq + Hash + Clone,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for State<T>
where
    T: Eq + Hash + Clone,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // We want the smallest cost + heuristic to be at the top of the heap.
        (self.cost + self.heuristic)
            .cmp(&(other.cost + other.heuristic))
            .reverse()
    }
}

impl<T> State<T>
where
    T: Eq + Hash + Clone,
{
    fn new(node: T, cost: usize, heuristic: usize) -> Self {
        State {
            node,
            cost,
            heuristic,
        }
    }
}

fn dijkstra<T, F, S, H>(
    start: &T,
    successors: F,
    success: S,
    heuristic: H,
) -> Option<(T, usize, usize)>
where
    T: Eq + Hash + Clone,
    F: Fn(&T) -> Vec<(T, usize)>,
    S: Fn(&T) -> bool,
    H: Fn(&T) -> usize,
{
    let mut visited = HashMap::new();
    let mut frontier = BinaryHeap::new();
    frontier.push(State::new(start.clone(), 0, heuristic(start)));

    while let Some(State {
        node,
        cost,
        heuristic: _,
    }) = frontier.pop()
    {
        // If we have reached our goal, return the node, cost, and number of visited nodes.
        if success(&node) {
            return Some((node, cost, visited.len()));
        }

        // If we have already visited this node and it was cheaper, skip it.
        if let Some(&prev_cost) = visited.get(&node) {
            if cost >= prev_cost {
                continue;
            }
        } else {
            visited.insert(node.clone(), cost);
        }

        // Add all the successors of the current node to the frontier.
        for (next, new_cost) in successors(&node) {
            frontier.push(State::new(next.clone(), cost + new_cost, heuristic(&next)));
        }
    }

    // No path found.
    None
}
