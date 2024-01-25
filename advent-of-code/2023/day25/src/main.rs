use std::collections::{HashMap, HashSet, VecDeque};

use priority_queue::PriorityQueue;
use rustworkx_core::{connectivity::stoer_wagner_min_cut, petgraph::prelude::UnGraph};

// We'll create a graph to simplify graph interactions with the
// algorithm.
#[derive(Clone)]
struct Graph<'a> {
    // A list of all nodes in the graph.
    nodes: HashSet<&'a str>,

    // All of the edges in the graph.
    edges: HashMap<&'a str, HashMap<&'a str, usize>>,
}

impl<'a> Graph<'a> {
    fn new(input: &'a str) -> Self {
        // Build the graph from our input.
        let mut graph = Graph {
            nodes: HashSet::new(),
            edges: HashMap::new(),
        };
        for line in input.lines() {
            let mut parts = line.split(": ");
            let node = parts.next().unwrap();
            let edges = parts.next().unwrap().split(' ');
            for edge in edges {
                graph.add_edge(node, edge, 1);
            }
        }
        graph
    }

    fn add_edge(&mut self, s: &'a str, t: &'a str, w: usize) {
        self.nodes.insert(s);
        self.nodes.insert(t);

        // We add it both directions here because it's an undirected graph.
        self.edges.entry(s).or_default().insert(t, w);
        self.edges.entry(t).or_default().insert(s, w);
    }

    fn get_edges(&self, node: &'a str) -> Vec<(&'a str, usize)> {
        // Get all edges from the given node and their weights.
        self.edges
            .get(node)
            .unwrap()
            .iter()
            .map(|(node, weight)| (*node, *weight))
            .collect()
    }

    fn update_edge(&mut self, s: &'a str, t: &'a str, w: usize) {
        // This is used during the contraction phase to update the
        // edge weights. We want to update in both directions.
        *self.edges.entry(s).or_default().entry(t).or_insert(0) += w;
        *self.edges.entry(t).or_default().entry(s).or_insert(0) += w;
    }

    fn remove(&mut self, node: &'a str) {
        // Remove the node and it's edges from the graph.
        self.nodes.remove(node);
        self.edges.remove(node);

        // We also need to find it's edges in all other nodes and
        // remove it there.
        self.edges.iter_mut().for_each(|(_, edges)| {
            edges.remove(node);
        });
    }
}

fn minimum_cut_phase<'a>(graph: &Graph<'a>, verbose: bool) -> (&'a str, &'a str, usize) {
    // We'll use a priority queue to find the "most tightly connected"
    // node. See the second paragraph of the "Running Time" section.
    let mut queue = PriorityQueue::new();
    graph.nodes.iter().for_each(|node| {
        queue.push(*node, 0);
    });

    // Track our last phase as it will be the set we return.
    let mut cut_weight = 0;
    let mut s = "-";
    let mut t = "-";

    // We'll slowly pop items out of the queue. Eventually we'll get
    // the last two items and they'll be the s and t we return.
    while let Some((node, weight)) = queue.pop() {
        // Update our trackers
        s = t;
        t = node;
        cut_weight = weight;

        // Now we need to update the edges in our priority queue so
        // the next queue pop gets the "most tightly connected".
        for (edge, weight) in graph.get_edges(node) {
            queue.change_priority_by(edge, |cur| *cur += weight);
        }

        if verbose {
            println!("  queue: {} {} {}", s, t, cut_weight);
        }
    }

    if verbose {
        println!("phase: {} {} {}", s, t, cut_weight);
    }
    (s, t, cut_weight)
}

// https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm
// https://dl.acm.org/doi/pdf/10.1145/263867.263872
fn stoer_wagner<'a>(graph: &'a Graph, verbose: bool) -> (usize, Vec<&'a str>) {
    // Make a copy of the graph so we can perform contractions on it.
    let mut contracted_graph = graph.clone();

    // Track the best phase and cut value and the contractions done
    // during each phase.
    let mut best_phase = 0;
    let mut best_cut_weight = usize::MAX;
    let mut contractions = Vec::new();

    // We are going to contract the gaph until we have only one node.
    for phase in 0..graph.nodes.len() - 1 {
        // Perform the minimum cut phase.
        let (s, t, cut_weight) = minimum_cut_phase(&contracted_graph, verbose);

        // If this is the best phase, save it.
        if cut_weight < best_cut_weight {
            best_phase = phase;
            best_cut_weight = cut_weight;
        }

        // Add our contraction to the list.
        contractions.push((s, t));

        // Perform the contraction. This is essentially getting all
        // the nodes linked to t and linking them to s. If one node is
        // connected to both, then the new weight is the sum of both.
        for (node, cost) in contracted_graph.get_edges(t) {
            contracted_graph.update_edge(s, node, cost);
        }

        // Remove the contracted node.
        contracted_graph.remove(t);
    }

    // We don't have a list of contractions. In the original paper,
    // the partitions were tracked at eah phase. Instead of doing
    // that, we can create a graph of the contractions up to the best
    // phase and then do a bfs to find the partition.
    //
    // This works becaus the contractions can be thought of as merging
    // the nodes. So the links between all merged nodes would be the
    // partition.
    if verbose {
        println!();
        println!("best-phase: {}", best_phase);
        println!("contractions: {:?}", contractions[..best_phase].to_vec());
        println!();
    }

    // Make the graph.
    let mut graph = HashMap::new();
    for (s, t) in contractions.iter().take(best_phase) {
        graph.entry(*s).or_insert_with(Vec::new).push(*t);
        graph.entry(*t).or_insert_with(Vec::new).push(*s);
    }

    // Do a bfs.
    let mut visited = HashSet::new();
    let mut frontier = VecDeque::new();
    frontier.push_back(contractions[best_phase].1);
    while let Some(node) = frontier.pop_front() {
        if visited.contains(node) {
            continue;
        }
        visited.insert(node);
        if let Some(edges) = graph.get(node) {
            for edge in edges {
                frontier.push_back(*edge);
            }
        }
    }

    // The partition is the visited nodes.
    (best_cut_weight, visited.into_iter().collect())
}

fn stoer_wagner_fast(graph: &Graph) -> usize {
    // Make a copy of the graph so we can perform contractions on it.
    let mut contracted_graph = graph.clone();

    // Track the best phase and cut value and the contractions done
    // during each phase.
    let mut contractions = HashMap::new();

    // We are going to contract the gaph until we have only one node.
    for _ in 0..graph.nodes.len() - 1 {
        // Perform the minimum cut phase.
        let (s, t, cut_weight) = minimum_cut_phase(&contracted_graph, false);

        // Add our contraction to the list.
        contractions.entry(s).or_insert_with(Vec::new).push(t);
        if cut_weight == 3 {
            let count = contractions_recursive(&contractions, t);
            return count * (graph.nodes.len() - count);
        }

        // Perform the contraction. This is essentially getting all
        // the nodes linked to t and linking them to s. If one node is
        // connected to both, then the new weight is the sum of both.
        for (node, cost) in contracted_graph.get_edges(t) {
            contracted_graph.update_edge(s, node, cost);
        }

        // Remove the contracted node.
        contracted_graph.remove(t);
    }

    0
}

fn contractions_recursive(contractions: &HashMap<&str, Vec<&str>>, node: &str) -> usize {
    let mut count = 1;
    if let Some(edges) = contractions.get(node) {
        for edge in edges {
            count += contractions_recursive(contractions, edge);
        }
    }
    count
}

fn main() {
    // Example graph from paper. We pick nodes to start arbitratily,
    // so it won't look quite the same, but it should give you an idea
    // of what the algorithm is doing.
    // let mut graph = Graph::new("");
    // graph.add_edge("A", "B", 2);
    // graph.add_edge("A", "E", 3);
    // graph.add_edge("B", "E", 3);
    // graph.add_edge("B", "C", 3);
    // graph.add_edge("B", "F", 2);
    // graph.add_edge("C", "G", 2);
    // graph.add_edge("C", "D", 4);
    // graph.add_edge("F", "E", 3);
    // graph.add_edge("F", "G", 1);
    // graph.add_edge("G", "D", 2);
    // graph.add_edge("D", "H", 2);
    // graph.add_edge("G", "H", 3);

    // let (cut, partition) = stoer_wagner(&graph, true);
    // println!("example:");
    // println!("minimum-cut: {}", cut);
    // println!("partition: {:?}", partition);
    // println!();

    // Build a graph and run our version of Stoer-Wagner.
    let input = std::fs::read_to_string("input").unwrap();
    let now = std::time::Instant::now();
    let graph = Graph::new(&input);
    let (cut, partition) = stoer_wagner(&graph, false);
    // println!("stoer-wagner:");
    // println!("minimum-cut: {}", cut);
    let p1 = partition.len() * (graph.nodes.len() - partition.len());
    println!("p1: {} ({:?})", p1, now.elapsed());
    // println!();

    let now = std::time::Instant::now();
    let graph = Graph::new(&input);
    println!(
        "stoer-wagner-fast: {} ({:?})",
        stoer_wagner_fast(&graph),
        now.elapsed()
    );

    // Use rustworkx to build a graph and run Stoer-Wagner.
    // let mut graph: UnGraph<&str, ()> = rustworkx_core::petgraph::Graph::new_undirected();
    // let mut nodes = HashMap::new();
    // for line in input.lines() {
    //     let mut parts = line.split(": ");
    //     let node = parts.next().unwrap();
    //     let node = *nodes.entry(node).or_insert_with(|| graph.add_node(node));
    //     let edges = parts.next().unwrap().split(' ');
    //     for edge in edges {
    //         let edge = *nodes.entry(edge).or_insert_with(|| graph.add_node(edge));
    //         graph.add_edge(node, edge, ());
    //     }
    // }

    // println!("rustworkx:");
    // let now = std::time::Instant::now();
    // match stoer_wagner_min_cut(&graph, |_| Ok::<i32, ()>(1)) {
    //     Err(_) => unreachable!(),
    //     Ok(None) => println!("no solution found"),
    //     Ok(Some((cut, partition))) => {
    //         println!("rustworkx-minmum-cut: {}", cut);
    //         let p1 = partition.len() * (nodes.len() - partition.len());
    //         println!("p1-rustworkx: {} ({:?})", p1, now.elapsed());
    //     }
    // }
}
