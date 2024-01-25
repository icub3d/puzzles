use std::{
    collections::{BinaryHeap, HashSet},
    hash::{Hash, Hasher},
};

#[derive(Clone, Debug, Eq)]
struct Building {
    generators: Vec<usize>,
    microchips: Vec<usize>,
    elevator: usize,
}

impl PartialEq for Building {
    fn eq(&self, other: &Self) -> bool {
        let generators = self.generators.iter().fold([0; 4], |mut acc, &x| {
            acc[x] += 1;
            acc
        });
        let other_generators = other.generators.iter().fold([0; 4], |mut acc, &x| {
            acc[x] += 1;
            acc
        });

        if generators != other_generators {
            return false;
        }

        let microchips = self.microchips.iter().fold([0; 4], |mut acc, &x| {
            acc[x] += 1;
            acc
        });
        let other_microchips = other.microchips.iter().fold([0; 4], |mut acc, &x| {
            acc[x] += 1;
            acc
        });
        if microchips != other_microchips {
            return false;
        }
        self.elevator == other.elevator
    }
}

impl Hash for Building {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.generators
            .iter()
            .fold([0; 4], |mut acc, &x| {
                acc[x] += 1;
                acc
            })
            .iter()
            .for_each(|&x| state.write_usize(x));
        self.microchips
            .iter()
            .fold([0; 4], |mut acc, &x| {
                acc[x] += 1;
                acc
            })
            .iter()
            .for_each(|&x| state.write_usize(x));
        state.write_usize(self.elevator);
    }
}

impl Building {
    fn new(pairs: usize) -> Self {
        Self {
            generators: vec![0; pairs],
            microchips: vec![0; pairs],
            elevator: 0,
        }
    }

    fn valid(&self) -> bool {
        self.generators
            .iter()
            .zip(self.microchips.iter())
            .all(|(g, m)| g == m || self.generators.iter().all(|g| g != m))
    }

    fn complete(&self) -> bool {
        self.generators.iter().all(|c| *c == 3) && self.microchips.iter().all(|c| *c == 3)
    }

    fn neighbors(&self) -> Vec<Self> {
        let mut neighbors = Vec::new();
        let new_floors = match self.elevator {
            0 => vec![1],
            3 => vec![2],
            _ => vec![self.elevator + 1, self.elevator - 1],
        };

        // We can do all generates by themselves.
        for i in 0..self.generators.len() {
            if self.generators[i] != self.elevator {
                continue;
            }
            for new_floor in &new_floors {
                let mut neighbor = self.clone();
                neighbor.generators[i] = *new_floor;
                neighbor.elevator = *new_floor;
                if neighbor.valid() {
                    neighbors.push(neighbor);
                }
            }

            // Also try this generator with other generators.
            for j in i + 1..self.generators.len() {
                if self.generators[j] != self.elevator {
                    continue;
                }
                for new_floor in &new_floors {
                    let mut neighbor = self.clone();
                    neighbor.generators[i] = *new_floor;
                    neighbor.generators[j] = *new_floor;
                    neighbor.elevator = *new_floor;
                    if neighbor.valid() {
                        neighbors.push(neighbor);
                    }
                }
            }
        }

        // We can do all microchips by themselves.
        for i in 0..self.microchips.len() {
            if self.microchips[i] != self.elevator {
                continue;
            }
            for new_floor in &new_floors {
                let mut neighbor = self.clone();
                neighbor.microchips[i] = *new_floor;
                neighbor.elevator = *new_floor;
                if neighbor.valid() {
                    neighbors.push(neighbor);
                }
            }

            // Also try this microchip with other microchips.
            for j in i + 1..self.microchips.len() {
                if self.microchips[j] != self.elevator {
                    continue;
                }
                for new_floor in &new_floors {
                    let mut neighbor = self.clone();
                    neighbor.microchips[i] = *new_floor;
                    neighbor.microchips[j] = *new_floor;
                    neighbor.elevator = *new_floor;
                    if neighbor.valid() {
                        neighbors.push(neighbor);
                    }
                }
            }
        }

        // Finally, we can do a generator and a microchip if they are
        // the same type and on this floor.
        for i in 0..self.generators.len() {
            if self.generators[i] != self.elevator {
                continue;
            }
            for j in 0..self.microchips.len() {
                if self.microchips[j] != self.elevator || self.generators[j] != self.elevator {
                    continue;
                }
                for new_floor in &new_floors {
                    let mut neighbor = self.clone();
                    neighbor.generators[i] = *new_floor;
                    neighbor.microchips[j] = *new_floor;
                    neighbor.elevator = *new_floor;
                    if neighbor.valid() {
                        neighbors.push(neighbor);
                    }
                }
            }
        }

        neighbors
    }

    fn distance_to_goal(&self) -> usize {
        self.generators.len() * 4 + self.microchips.len() * 4
            - self.generators.iter().sum::<usize>()
            - self.microchips.iter().sum::<usize>()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct State {
    steps: usize,
    building: Building,
}

impl State {
    fn new(building: Building, steps: usize) -> Self {
        Self { steps, building }
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.steps.cmp(&self.steps)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn dijkstra(start: &Building) -> Option<(usize, usize)> {
    let mut queue = BinaryHeap::new();
    let mut visited = HashSet::new();
    queue.push(State::new(start.clone(), 0));
    visited.insert(start.clone());
    let mut counter = 0;
    while let Some(State {
        building, steps, ..
    }) = queue.pop()
    {
        counter += 1;
        if building.complete() {
            return Some((steps, counter));
        }
        for neighbor in building.neighbors() {
            if !visited.contains(&neighbor) {
                queue.push(State::new(neighbor.clone(), steps + 1));
                visited.insert(neighbor.clone());
            }
        }
    }
    None
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct StateWithHeuristic {
    heuristic: usize,
    state: State,
}

impl StateWithHeuristic {
    fn new(building: Building, steps: usize) -> Self {
        Self {
            heuristic: building.distance_to_goal(),
            state: State::new(building, steps),
        }
    }
}

impl Ord for StateWithHeuristic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (other.state.steps + other.heuristic).cmp(&(self.state.steps + self.heuristic))
    }
}

impl PartialOrd for StateWithHeuristic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn astar(start: &Building) -> Option<(usize, usize)> {
    let mut queue = BinaryHeap::new();
    let mut visited = HashSet::new();
    queue.push(StateWithHeuristic::new(start.clone(), 0));
    visited.insert(start.clone());
    let mut counter = 0;
    while let Some(StateWithHeuristic {
        state: State { building, steps },
        ..
    }) = queue.pop()
    {
        counter += 1;
        if building.complete() {
            return Some((steps, counter));
        }
        for neighbor in building.neighbors() {
            if !visited.contains(&neighbor) {
                queue.push(StateWithHeuristic::new(neighbor.clone(), steps + 1));
                visited.insert(neighbor.clone());
            }
        }
    }
    None
}

fn main() {
    let mut start = Building::new(2);
    start.generators[0] = 1;
    start.generators[1] = 2;
    let now = std::time::Instant::now();
    println!("test-astar:     {:?} ({:?})", astar(&start), now.elapsed());
    let now = std::time::Instant::now();
    println!(
        "test-dijkstra:  {:?} ({:?})",
        dijkstra(&start),
        now.elapsed()
    );
    println!();

    let mut start = Building::new(5);
    start.microchips[0] = 1;
    start.microchips[1] = 1;
    let now = std::time::Instant::now();
    println!("p1-astar:       {:?} ({:?})", astar(&start), now.elapsed());
    let now = std::time::Instant::now();
    println!(
        "p1-dijkstra:    {:?} ({:?})",
        dijkstra(&start),
        now.elapsed()
    );
    println!();

    let mut start = Building::new(7);
    start.microchips[0] = 1;
    start.microchips[1] = 1;
    let now = std::time::Instant::now();
    println!("p2-astar:       {:?} ({:?})", astar(&start), now.elapsed());
    let now = std::time::Instant::now();
    println!(
        "p2-dijkstra:    {:?} ({:?})",
        dijkstra(&start),
        now.elapsed()
    );
}
