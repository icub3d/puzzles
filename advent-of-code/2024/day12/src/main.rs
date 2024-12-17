use std::{
    collections::{HashMap, HashSet},
    ops::Add,
};

// Used when finding a region. We do a bfs to find adjacent plots with the same
// plant.
static NEIGHBORS: [Plot; 4] = [
    Plot { x: 0, y: -1 },
    Plot { x: 1, y: 0 },
    Plot { x: 0, y: 1 },
    Plot { x: -1, y: 0 },
];

// Used when finding corners. The first value is opposite the plot and the
// second and third are adjacent.
static CORNERS: [[Plot; 3]; 4] = [
    [
        Plot { x: -1, y: -1 },
        Plot { x: -1, y: 0 },
        Plot { x: 0, y: -1 },
    ],
    [
        Plot { x: 1, y: -1 },
        Plot { x: 1, y: 0 },
        Plot { x: 0, y: -1 },
    ],
    [
        Plot { x: 1, y: 1 },
        Plot { x: 1, y: 0 },
        Plot { x: 0, y: 1 },
    ],
    [
        Plot { x: -1, y: 1 },
        Plot { x: -1, y: 0 },
        Plot { x: 0, y: 1 },
    ],
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Plot {
    x: isize,
    y: isize,
}

impl Plot {
    fn new(x: usize, y: usize) -> Plot {
        Plot {
            x: x as isize,
            y: y as isize,
        }
    }
}

impl Add for Plot {
    type Output = Plot;

    fn add(self, other: Plot) -> Plot {
        Plot {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Add<Plot> for &Plot {
    type Output = Plot;

    fn add(self, other: Plot) -> Plot {
        Plot {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

struct Garden {
    plots: HashMap<Plot, char>,
}

impl Garden {
    fn new(input: &str) -> Garden {
        let plots = input
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars()
                    .enumerate()
                    .map(move |(x, plant)| (Plot::new(x, y), plant))
            })
            .collect();
        Garden { plots }
    }

    fn get(&self, plot: &Plot) -> Option<char> {
        self.plots.get(&plot).cloned()
    }

    // Find all neighbors of a plot that are in the garden.
    fn neighbors(&self, plot: &Plot) -> Vec<Plot> {
        NEIGHBORS
            .iter()
            .map(|dir| plot + *dir)
            .filter(|neighbor| self.plots.contains_key(neighbor))
            .collect()
    }

    // Find the number of corners that this plot contributes to the shape of the
    // region.
    fn corners(&self, plot: &Plot) -> usize {
        CORNERS
            .iter()
            .filter(|corner| {
                let opposite = self.get(&(plot + corner[0]));
                let first = self.get(&(plot + corner[1]));
                let second = self.get(&(plot + corner[2]));
                let plot = self.get(plot);
                // This corner of the plot is a corner of the shape if both
                // adjacent are different.  It is also a corner if adjacent are
                // the same and the opposite is different.
                (plot != second && plot != first)
                    || (plot == second && plot == first && plot != opposite)
            })
            .count()
    }
}

fn main() {
    let input = include_str!("input.txt");
    let garden = Garden::new(input);

    // Find the regions.
    let mut regions = Vec::new();
    let mut visited: HashSet<Plot> = HashSet::new();

    // Find the regions using a bfs.
    for plot in garden.plots.keys() {
        if visited.contains(plot) {
            continue;
        }
        let mut region = HashSet::new();
        find_region(&garden, plot, garden.plots[plot], &mut region);
        visited.extend(&region);
        regions.push(region);
    }

    // Calculate the cost.
    let now = std::time::Instant::now();
    let p1 = regions
        .iter()
        .map(|region| {
            // A plot can contribute up to 4 sides to the perimeter. Any
            // adjacent plots of the same type mean that side isn't part of the
            // perimeter.
            let perimeter = region
                .iter()
                .map(|plot| {
                    4 - garden
                        .neighbors(plot)
                        .iter()
                        .filter(|neighbor| garden.plots[neighbor] == garden.plots[plot])
                        .count()
                })
                .sum::<usize>();
            region.len() * perimeter
        })
        .sum::<usize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // Shout out to my daughter Anna and her homework for teaching me that sides == corners.
    let now = std::time::Instant::now();
    let p2 = regions
        .iter()
        .map(|region| {
            // Get the total number of corners and multiply by area to get cost.
            region
                .iter()
                .map(|plot| garden.corners(plot))
                .sum::<usize>()
                * region.len()
        })
        .sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
}

// Find the rest of a region given a current position and plant type. This is a bfs/flood fill.
fn find_region(garden: &Garden, plot: &Plot, plant: char, region: &mut HashSet<Plot>) {
    region.insert(*plot);
    for neighbor in garden.neighbors(plot) {
        if garden.plots[&neighbor] == plant && !region.contains(&neighbor) {
            find_region(garden, &neighbor, plant, region);
        }
    }
}
