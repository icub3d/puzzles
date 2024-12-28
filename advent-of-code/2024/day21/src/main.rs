use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::OnceLock,
};

use cached::proc_macro::cached;
use itertools::Itertools;

// The next two functions basically find all the possible shortest paths between
// any two points on each of the keypads. Because the combinations are small, we
// can pre-compute.
//
// We use OnceLock here and in the next function (previously would be
// lazy_static) so we can use the cached macro.  There may be an easier way to
// do this but it is nice that the cached macro has a reduced parameter list.
fn numeric_paths() -> &'static HashMap<(char, char), Vec<String>> {
    static NUMERIC_PATHS: OnceLock<HashMap<(char, char), Vec<String>>> = OnceLock::new();
    NUMERIC_PATHS.get_or_init(|| {
        // The first value if the button, the second is a list of its neighbors
        // and the direction you'd need to go to get to it.
        let numeric_keypad = vec![
            ('7', vec![('4', 'v'), ('8', '>')]),
            ('8', vec![('5', 'v'), ('9', '>'), ('7', '<')]),
            ('9', vec![('6', 'v'), ('8', '<')]),
            ('4', vec![('1', 'v'), ('5', '>'), ('7', '^')]),
            ('5', vec![('2', 'v'), ('6', '>'), ('4', '<'), ('8', '^')]),
            ('6', vec![('3', 'v'), ('5', '<'), ('9', '^')]),
            ('1', vec![('2', '>'), ('4', '^')]),
            ('2', vec![('3', '>'), ('5', '^'), ('1', '<'), ('0', 'v')]),
            ('0', vec![('2', '^'), ('A', '>')]),
            ('3', vec![('6', '^'), ('2', '<'), ('A', 'v')]),
            ('A', vec![('0', '<'), ('3', '^')]),
        ]
        .into_iter()
        .collect::<HashMap<char, Vec<(char, char)>>>();

        // For each combination of buttons, find the shortest paths between them.
        numeric_keypad
            .keys()
            .cartesian_product(numeric_keypad.keys())
            .map(|(&a, &b)| ((a, b), find_shortest_paths(&numeric_keypad, a, b)))
            .collect()
    })
}

fn direction_paths() -> &'static HashMap<(char, char), Vec<String>> {
    static DIRECTION_PATHS: OnceLock<HashMap<(char, char), Vec<String>>> = OnceLock::new();
    DIRECTION_PATHS.get_or_init(|| {
        let direction_keypad = vec![
            ('^', vec![('A', '>'), ('v', 'v')]),
            ('A', vec![('^', '<'), ('>', 'v')]),
            ('>', vec![('A', '^'), ('v', '<')]),
            ('<', vec![('v', '>')]),
            ('v', vec![('<', '<'), ('^', '^'), ('>', '>')]),
        ]
        .into_iter()
        .collect::<HashMap<char, Vec<(char, char)>>>();

        direction_keypad
            .keys()
            .cartesian_product(direction_keypad.keys())
            .map(|(&a, &b)| ((a, b), find_shortest_paths(&direction_keypad, a, b)))
            .collect()
    })
}

// Find all the shortest paths between two points on a keypad. This is a bfs
// that visits all nodes.
fn find_shortest_paths(
    neighbors: &HashMap<char, Vec<(char, char)>>,
    start: char,
    end: char,
) -> Vec<String> {
    // We use a queue here to do a breadth-first search of the keypad.
    let mut queue = VecDeque::new();
    queue.push_back((start, Vec::new(), HashSet::new()));

    // Track the paths we've found so far and the length of the shortest path.
    let mut paths = Vec::new();
    let mut lowest = std::usize::MAX;

    // While we have nodes to visit, keep looking for the end.
    while let Some((node, path, mut visited)) = queue.pop_front() {
        // If we found the end, add the path to the list of paths if it's part
        // of the lowest.
        if node == end {
            if path.len() <= lowest {
                lowest = path.len();
                paths.push(path.iter().collect::<String>());
            }
            continue;
        }

        // Check to see if we have already visited this node. If we are
        // continuing, add it to our visited set.
        if visited.contains(&node) {
            continue;
        }
        visited.insert(node);

        // For each neighbor create a new path that includes it and add to our queue.
        for (next, dir) in neighbors.get(&node).unwrap() {
            let mut path = path.clone();
            path.push(*dir);
            queue.push_back((*next, path, visited.clone()));
        }
    }

    paths
}

fn main() {
    let input = include_str!("input.txt");

    // We want to find the shortest sequence and then multiply it by the number
    // at the beginning of the line.
    let now = std::time::Instant::now();
    let p1 = input
        .lines()
        .map(|line| {
            find_shortest_sequence(line.to_string(), 2, true)
                * line.trim_end_matches('A').parse::<usize>().unwrap()
        })
        .sum::<usize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();
    let p2 = input
        .lines()
        .map(|line| {
            find_shortest_sequence(line.to_string(), 25, true)
                * line.trim_end_matches('A').parse::<usize>().unwrap()
        })
        .sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());
}

#[cached]
fn find_shortest_sequence(sequence: String, depth: usize, numeric: bool) -> usize {
    // Pick the right keypad paths.
    let paths = if numeric {
        numeric_paths()
    } else {
        direction_paths()
    };

    // We want to find the path from each button to the next. All robots start
    // at 'A', so we prefix the windows with that.
    ("A".to_string() + &sequence)
        .chars()
        .tuple_windows()
        .map(|(a, b)| {
            let shortest_paths = paths.get(&(a, b)).unwrap();
            match depth {
                // If we've reached the end, we just use the shortest path length.
                0 => shortest_paths[0].len() + 1,
                // Otherwise, we need to find the smallest path among all the paths.
                _ => shortest_paths
                    .iter()
                    .cloned()
                    .map(|mut path| {
                        // We put and 'A' at the end because they'll need to
                        // hit the 'A' button to tell the next in line to push their button.
                        path.push('A');
                        find_shortest_sequence(path, depth - 1, false)
                    })
                    .min()
                    .unwrap(),
            }
        })
        .sum::<usize>()
}
