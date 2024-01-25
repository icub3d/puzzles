// https://adventofcode.com/2022/day/19
// https://en.wikipedia.org/wiki/Branch_and_bound

use std::ops::{Add, Index, IndexMut, Mul, Sub};

// We are going to use rayon here to parallelize the work. This is
// especially beneficial for part 1 where there are many blueprints.
use rayon::prelude::*;

// These are the different mineral types. We'll use them to iterate.
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Mineral {
    Ore,
    Clay,
    Obsidian,
    Geode,
}

// The below impls just allow use to use the Mineral enum as an index.

impl Index<Mineral> for [usize; 4] {
    type Output = usize;
    fn index(&self, mineral: Mineral) -> &Self::Output {
        &self[mineral as usize]
    }
}

impl Index<Mineral> for MineralSet {
    type Output = usize;
    fn index(&self, mineral: Mineral) -> &Self::Output {
        &self.minerals[mineral as usize]
    }
}

impl IndexMut<Mineral> for MineralSet {
    fn index_mut(&mut self, mineral: Mineral) -> &mut Self::Output {
        &mut self.minerals[mineral as usize]
    }
}

impl Index<Mineral> for Blueprint {
    type Output = MineralSet;
    fn index(&self, mineral: Mineral) -> &Self::Output {
        &self.costs[mineral as usize]
    }
}

// This is a set of minerals. We'll use it to track how many minerals
// we have, how much robots cust, etc.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct MineralSet {
    minerals: [usize; 4],
}

impl MineralSet {
    fn le(&self, other: &Self) -> bool {
        // Do a custom comparison for checking that we have enough
        // minerals to build a robot. Implementing PartialOrd feels a
        // bit strange because we are interested in all being less
        // than or equal. That being said, it's not used anywhere
        // else. Thoughts?
        self.minerals[0] <= other.minerals[0]
            && self.minerals[1] <= other.minerals[1]
            && self.minerals[2] <= other.minerals[2]
    }
}

// This allows use to use into() below.
impl From<[usize; 4]> for MineralSet {
    fn from(minerals: [usize; 4]) -> Self {
        Self { minerals }
    }
}

// Implement some basic math operations for MineralSet to do our
// calculations.

impl Add for MineralSet {
    type Output = Self;
    fn add(self, other: Self) -> Self::Output {
        Self {
            minerals: [
                self.minerals[0] + other.minerals[0],
                self.minerals[1] + other.minerals[1],
                self.minerals[2] + other.minerals[2],
                self.minerals[3] + other.minerals[3],
            ],
        }
    }
}

impl Mul<usize> for MineralSet {
    type Output = Self;
    fn mul(self, other: usize) -> Self::Output {
        Self {
            minerals: [
                self.minerals[0] * other,
                self.minerals[1] * other,
                self.minerals[2] * other,
                self.minerals[3] * other,
            ],
        }
    }
}

impl Sub for MineralSet {
    type Output = Self;
    fn sub(self, other: Self) -> Self::Output {
        Self {
            minerals: [
                self.minerals[0] - other.minerals[0],
                self.minerals[1] - other.minerals[1],
                self.minerals[2] - other.minerals[2],
                self.minerals[3] - other.minerals[3],
            ],
        }
    }
}

#[derive(Debug, Clone)]
struct Blueprint {
    costs: [MineralSet; 4],
}

impl From<&str> for Blueprint {
    fn from(input: &str) -> Self {
        // Parse a line from the input file into a Blueprint. Note
        // that the blueprint ID isn't here because filter_map will
        // ignore it because it doesn't parse.
        let parts = input
            .split(' ')
            .filter_map(|s| s.parse::<usize>().ok())
            .collect::<Vec<_>>();

        // The coste types are static in the whole file, so we can
        // just hardcode them here.
        Self {
            costs: [
                MineralSet::from([parts[0], 0, 0, 0]),
                MineralSet::from([parts[1], 0, 0, 0]),
                MineralSet::from([parts[2], parts[3], 0, 0]),
                MineralSet::from([parts[4], 0, parts[5], 0]),
            ],
        }
    }
}

// Track a state of the simulation.
#[derive(Debug, Clone)]
struct State<'a> {
    // How many minutes are left in the simulation.
    remaining: usize,

    // The blueprint we are working on.
    blueprint: &'a Blueprint,

    // The bank of minerals we have.
    bank: MineralSet,

    // The number of robots we have.
    robots: MineralSet,
}

impl<'a> State<'a> {
    fn new(blueprint: &'a Blueprint, remaining: usize) -> Self {
        Self {
            remaining,
            blueprint,
            bank: [0; 4].into(),
            // We start with one ore robot.
            robots: [1, 0, 0, 0].into(),
        }
    }

    fn next(&self) -> Vec<State> {
        // Calculate the next states. The branching occurs when we
        // have a new robot of a different type. So, we want to find
        // the point at which we could create a new robot of each
        // type.
        let mut states = Vec::new();
        for mineral in [
            Mineral::Ore,
            Mineral::Clay,
            Mineral::Obsidian,
            Mineral::Geode,
        ] {
            // I tried a bunch of math stuff but could never get it to
            // work. If you can figure out the math, please let me
            // know! We'll just loop through time until we can create
            // one.
            let mut state = self.clone();
            for next in 1..self.remaining {
                if !self.blueprint[mineral].le(&state.bank) {
                    // If we don't have enough, keep going forward in
                    // time.
                    state.bank = state.bank + state.robots;
                    continue;
                }
                // Otherwise, we can create a new robot and add that
                // updated state to our list.
                state.remaining -= next;
                state.bank = state.bank + state.robots;
                state.bank = state.bank - self.blueprint[mineral];
                state.robots[mineral] += 1;
                states.push(state);
                break;
            }
        }
        states
    }

    #[allow(dead_code)]
    fn heuristic_none(&self) -> usize {
        // This ensures that we always check the branch. That is,
        // there is no "bounding".
        std::usize::MAX
    }

    fn heuristic_geode_robot_per_minute(&self) -> usize {
        // https://gist.github.com/Gordin508/5d08764a5f4d95ea8be4bfa3114189d7
        self.robots[Mineral::Geode] * self.remaining
            + (self.remaining * (self.remaining + 1) / 2 + self.bank[Mineral::Geode])
    }

    fn heuristic_focus_geode_robots(&self) -> usize {
        // This bounding focuses on maximizing the number of geode
        // robots. We ignore ore and clay here just assuming we always
        // have enough. So we only need to check if we have enough
        // obsidian to build a geode robot.
        let geode_robot_cost = self.blueprint[Mineral::Geode].minerals[Mineral::Obsidian];
        let mut geode_rate = self.robots[Mineral::Geode];
        let mut obsidian_rate = self.robots[Mineral::Obsidian];
        let mut obsidian = self.bank[Mineral::Obsidian];
        let mut geodes = self.bank[Mineral::Geode];

        for _ in 0..self.remaining {
            // We can only build one robot per minute, so we try to
            // build a geode robot first. If not, assume we can build
            // an obsidian robot.
            if geode_robot_cost <= obsidian {
                obsidian = (obsidian + obsidian_rate) - geode_robot_cost;
                geode_rate += 1;
            } else {
                obsidian += obsidian_rate;
                obsidian_rate += 1;
            }
            geodes += geode_rate;
        }

        geodes
    }
}

fn dfs<H>(state: &State, best: &mut usize, heuristic: &mut H)
where
    H: FnMut(&State) -> usize,
{
    // Update our best time.
    *best = std::cmp::max(
        *best,
        state.bank[Mineral::Geode] + state.robots[Mineral::Geode] * state.remaining,
    );

    // Bound - stop if this isn't going to improve.
    if heuristic(state) < *best {
        return;
    }

    // Branch - go down neighbor states.
    for state in state.next() {
        dfs(&state, best, heuristic);
    }
}

fn main() {
    // Collect our blueprints.
    let input = include_str!("../input");
    let blueprints = input.lines().map(Blueprint::from).collect::<Vec<_>>();

    // Let's do a comparison of not using a heuristic vs. using one.
    let blueprint = Blueprint::from("2 3 3 8 3 12");
    let state = State::new(&blueprint, 24);

    // Try with no heuristic.
    let now = std::time::Instant::now();
    let mut iterations = 0;
    let mut best = 0;
    dfs(&state, &mut best, &mut |state| {
        iterations += 1;
        state.heuristic_none()
    });
    println!("heuristic-none-24: ({:?})", now.elapsed());
    println!("iterations: {:>12}", iterations);
    println!("best:       {:>12}", best);
    println!();

    // Note: This took several minutes and didn't finish!
    // Try with no heuristic with longer time.
    // let state = State::new(&blueprint, 32);
    // let now = std::time::Instant::now();
    // let mut iterations = 0;
    // let mut best = 0;
    // dfs(&state, &mut best, &mut |state| {
    //     iterations += 1;
    //     state.heuristic_none()
    // });
    // println!("heuristic-none-32: ({:?})", now.elapsed());
    // println!("iterations: {:>12}", iterations);
    // println!("best:       {:>12}", best);
    // println!();

    // Try with Gordin508's heuristic.
    let now = std::time::Instant::now();
    let mut iterations = 0;
    let mut best = 0;
    dfs(&state, &mut best, &mut |state| {
        iterations += 1;
        state.heuristic_geode_robot_per_minute()
    });
    println!("heuristic-geode-robot-per-minute: ({:?})", now.elapsed());
    println!("iterations: {:>12}", iterations);
    println!("best:       {:>12}", best);
    println!();

    // Try with my heuristic.
    let now = std::time::Instant::now();
    let mut iterations = 0;
    let mut best = 0;
    dfs(&state, &mut best, &mut |state| {
        iterations += 1;
        state.heuristic_focus_geode_robots()
    });
    println!("heuristic-focus-geode-robots: ({:?})", now.elapsed());
    println!("iterations: {:>12}", iterations);
    println!("best:       {:>12}", best);
    println!();

    // For part 1, we'll just iterate over all the blueprints and find
    // the best and then use it to do the calculation for the quality.
    let now = std::time::Instant::now();
    let p1 = blueprints
        .par_iter()
        .map(|blueprint| State::new(blueprint, 24))
        .enumerate()
        .map(|(i, state)| {
            let mut best = 0;
            dfs(&state, &mut best, &mut |state| {
                state.heuristic_focus_geode_robots()
            });
            (i + 1) * best
        })
        .sum::<usize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // For part 2, we are only interested in the first three but now
    // want the product.
    let now = std::time::Instant::now();
    let p2 = blueprints
        .par_iter()
        .take(3)
        .map(|blueprint| State::new(blueprint, 32))
        .map(|state| {
            let mut best = 0;
            dfs(&state, &mut best, &mut |state| {
                state.heuristic_focus_geode_robots()
            });
            best
        })
        .product::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
}
