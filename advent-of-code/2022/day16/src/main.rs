use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap, HashSet},
    fs,
};

use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

// Valve NQ has flow rate=0; tunnels lead to valves SU, XD
lazy_static! {
    static ref RE: Regex =
        Regex::new("Valve ([^ ]*) has flow rate=([^;]*); tunnel[s]? lead[s]? to valve[s]? (.*)")
            .unwrap();
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct Valve<'a> {
    name: &'a str,
    flow_rate: usize,
    leads_to: Vec<&'a str>,
}

impl<'a> Valve<'a> {
    fn new(s: &'a str) -> Self {
        let caps: regex::Captures<'a> = RE.captures(s).unwrap();

        Self {
            name: caps.get(1).unwrap().as_str(),
            flow_rate: caps[2].parse::<usize>().unwrap(),
            leads_to: caps.get(3).unwrap().as_str().split(", ").collect(),
        }
    }
}

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let valves = lines
        .lines()
        .map(Valve::new)
        .map(|v| (v.name, v))
        .collect::<HashMap<&str, Valve>>();

    let mut non_zero = valves
        .values()
        .filter(|v| v.flow_rate > 0)
        .map(|v| v.name)
        .collect::<HashSet<&str>>();
    non_zero.insert("AA");

    // Determine the shortest distance between each of the non-zero items.
    let mut dist: HashMap<(&str, &str), usize> = HashMap::new();
    for first in &non_zero {
        for second in &non_zero {
            if first != second {
                dist.insert((first, second), dijkstra(&valves, first, second).unwrap());
            }
        }
    }

    non_zero.remove("AA");
    println!("p1: {}", max_pressure(&valves, &dist, "AA", 30, &non_zero));

    // For part 2, we just make different combinations of the non_zero values and find the max of them all.
    let non_zero: HashSet<&str> = non_zero.into_iter().collect();
    let mut max = 0;
    for i in 0..non_zero.len() {
        for c in non_zero.clone().into_iter().combinations(i) {
            let s1 = c.into_iter().collect::<HashSet<&str>>();
            let s2 = non_zero
                .difference(&s1)
                .map(|s| *s)
                .collect::<HashSet<&str>>();
            let b1 = max_pressure(&valves, &dist, "AA", 26, &s1);
            let b2 = max_pressure(&valves, &dist, "AA", 26, &s2);
            max = max.max(b1 + b2);
        }
    }
    println!("p2: {max}");
}

fn max_pressure<'a>(
    valves: &HashMap<&str, Valve>,
    dists: &HashMap<(&str, &str), usize>,
    cur: &'a str,
    time: usize,
    remaining: &HashSet<&'a str>,
) -> usize {
    // basically do a bfs here.
    let mut max = 0;
    for next in remaining.iter() {
        let d = dists[&(cur, *next)];
        if d + 1 > time {
            // Move and open.
            continue;
        }
        let time = time - (d + 1);
        let mut remaining = remaining.clone();
        remaining.remove(next);
        let m = (valves.get(next).unwrap().flow_rate * time)
            + max_pressure(valves, dists, next, time, &remaining);
        max = max.max(m);
    }
    max
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State<'a> {
    cost: usize,
    valve: &'a str,
}

impl<'a> Ord for State<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.valve.cmp(other.valve))
    }
}

impl<'a> PartialOrd for State<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn dijkstra(valves: &HashMap<&str, Valve>, start: &str, end: &str) -> Option<usize> {
    let mut dist: HashMap<&str, usize> = HashMap::new();
    let mut heap = BinaryHeap::new();

    // Setup our beginning position.
    dist.insert(start, 0);
    heap.push(State {
        cost: 0,
        valve: start,
    });
    while let Some(State { cost, valve }) = heap.pop() {
        // If we hit the end, we know the shortest distance now.
        if valve == end {
            return Some(cost);
        }

        // If we have a distance already and it's smaller, we don't
        // need to keep down this path.
        if cost > *dist.get(valve).unwrap_or(&usize::MAX) {
            continue;
        }

        // Loop through each of this points edges.
        for edge in &valves[valve].leads_to {
            let next = State {
                cost: cost + 1,
                valve: edge,
            };
            if next.cost < *dist.get(edge).unwrap_or(&usize::MAX) {
                heap.push(next);
                dist.insert(edge, next.cost);
            }
        }
    }
    None
}
