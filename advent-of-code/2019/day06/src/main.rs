use std::collections::{HashMap, HashSet};

fn main() {
    let input = include_str!("../input");
    let graph = input.lines().fold(HashMap::new(), |mut graph, line| {
        let mut parts = line.split(')');
        let parent = parts.next().unwrap();
        let child = parts.next().unwrap();
        graph.insert(child, parent);
        graph
    });

    let mut memo = HashMap::new();
    let p1 = graph
        .keys()
        .map(|k| count_orbits(k, &graph, &mut memo))
        .sum::<usize>();
    println!("p1: {}", p1);

    let you_path = path_to_com("YOU", &graph);
    let san_path = path_to_com("SAN", &graph);
    let p2 = you_path.symmetric_difference(&san_path).count();
    println!("p2: {}", p2);
}

fn path_to_com<'a>(node: &'a str, graph: &HashMap<&'a str, &'a str>) -> HashSet<&'a str> {
    let mut path = HashSet::new();
    let mut current = node;
    while let Some(parent) = graph.get(current) {
        path.insert(*parent);
        current = parent;
    }
    path
}

fn count_orbits<'a>(
    node: &'a str,
    graph: &'a HashMap<&'a str, &'a str>,
    memo: &mut HashMap<&'a str, usize>,
) -> usize {
    if let Some(count) = memo.get(node) {
        return *count;
    }

    let count = match graph.get(node) {
        Some(parent) => count_orbits(parent, graph, memo) + 1,
        None => 0,
    };

    memo.insert(node, count);
    count
}
