struct Solution;

#[derive(Clone, Debug, Eq, PartialEq)]
struct State {
    state: Vec<i32>,
    cost: i32,
}

impl State {
    fn new(state: Vec<i32>, cost: i32) -> Self {
        Self { state, cost }
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Again, reverse here for a min-heap.
        (other.cost).cmp(&(self.cost))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct StateWithHeuristic {
    state: State,
    heuristic: i32,
}

impl StateWithHeuristic {
    fn new(state: Vec<i32>, cost: i32, heuristic: i32) -> Self {
        Self {
            state: State::new(state, cost),
            heuristic,
        }
    }
}

impl Ord for StateWithHeuristic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Again, reverse here for a min-heap.
        (other.heuristic).cmp(&(self.heuristic))
    }
}

impl PartialOrd for StateWithHeuristic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Solution {
    fn neighbors(state: &[i32]) -> Vec<(Vec<i32>, i32)> {
        let mut neighbors = Vec::new();
        // For each wheel, we want to roll it forward and backward.
        for i in 0..state.len() {
            // Roll it one position forward.
            let mut neighbor = state.to_vec();
            neighbor[i] = (neighbor[i] + 1) % 10;
            neighbors.push((neighbor, 1));

            // Roll it one position backward.
            let mut neighbor = state.to_vec();
            neighbor[i] = (neighbor[i] + 9) % 10;
            neighbors.push((neighbor, 1));
        }
        neighbors
    }

    fn heuristic(state: &[i32], target: &[i32]) -> i32 {
        // For each digit, we calculate the distance between the
        // current digit and the target digit. It may be faster to
        // wrap around from 0 to 9 or 9 to 0.
        state
            .iter()
            .zip(target.iter())
            .map(|(a, b)| {
                let diff = (a - b).abs();
                std::cmp::min(diff, 10 - diff)
            })
            .sum()
    }

    pub fn open_lock(deadends: Vec<String>, target: String) -> i32 {
        // Turn our strings into vectors of digits.
        let start = vec![0, 0, 0, 0];
        let target: Vec<i32> = target
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i32)
            .collect();
        let deadends: Vec<Vec<i32>> = deadends
            .iter()
            .map(|deadend| {
                deadend
                    .chars()
                    .map(|c| c.to_digit(10).unwrap() as i32)
                    .collect()
            })
            .collect();

        let mut queue = std::collections::BinaryHeap::new();
        queue.push(StateWithHeuristic::new(
            start.clone(),
            0,
            Solution::heuristic(&start, &target),
        ));

        let mut distances = std::collections::HashMap::new();
        distances.insert(start.clone(), 0);

        let mut counter = 0;
        while let Some(StateWithHeuristic {
            state: State { state, cost },
            ..
        }) = queue.pop()
        {
            counter += 1;
            if state == target {
                println!("counter:  {}", counter);
                return cost;
            }
            if deadends.contains(&state) {
                continue;
            }
            for (neighbor, neighbor_cost) in Solution::neighbors(&state) {
                let new_cost = cost + neighbor_cost;
                if let Some(&best_cost) = distances.get(&neighbor) {
                    if best_cost <= new_cost {
                        continue;
                    }
                }

                // Add to our queue but now we include the heuristic.
                queue.push(StateWithHeuristic::new(
                    neighbor.clone(),
                    new_cost,
                    Solution::heuristic(&neighbor, &target),
                ));
                distances.insert(neighbor.clone(), new_cost);
            }
        }

        -1
    }

    pub fn open_lock_dijktra(deadends: Vec<String>, target: String) -> i32 {
        // Turn our strings into vectors of digits.
        let start = vec![0, 0, 0, 0];
        let target: Vec<i32> = target
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i32)
            .collect();
        let deadends: Vec<Vec<i32>> = deadends
            .iter()
            .map(|deadend| {
                deadend
                    .chars()
                    .map(|c| c.to_digit(10).unwrap() as i32)
                    .collect()
            })
            .collect();

        let mut queue = std::collections::BinaryHeap::new();
        queue.push(State::new(start.clone(), 0));

        let mut distances = std::collections::HashMap::new();
        distances.insert(start.clone(), 0);

        let mut counter = 0;
        while let Some(State { state, cost }) = queue.pop() {
            counter += 1;
            if state == target {
                println!("counter:  {}", counter);
                return cost;
            }
            if deadends.contains(&state) {
                continue;
            }
            for (neighbor, neighbor_cost) in Solution::neighbors(&state) {
                let new_cost = cost + neighbor_cost;
                if let Some(&best_cost) = distances.get(&neighbor) {
                    if best_cost <= new_cost {
                        continue;
                    }
                }

                // Add to our queue but now we include the heuristic.
                queue.push(State::new(neighbor.clone(), new_cost));
                distances.insert(neighbor.clone(), new_cost);
            }
        }

        -1
    }
}

fn main() {
    println!(
        "a-star:   {}",
        Solution::open_lock(
            vec![
                "0201".to_string(),
                "0101".to_string(),
                "0102".to_string(),
                "1212".to_string(),
                "2002".to_string()
            ],
            "0202".to_string()
        )
    );
    println!();
    println!(
        "dijkstra: {}",
        Solution::open_lock_dijktra(
            vec![
                "0201".to_string(),
                "0101".to_string(),
                "0102".to_string(),
                "1212".to_string(),
                "2002".to_string()
            ],
            "0202".to_string()
        )
    );
}
