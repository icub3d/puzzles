use std::collections::{HashMap, HashSet, VecDeque};

use itertools::Itertools;

fn main() {
    let now = std::time::Instant::now();
    let input = include_str!("input.txt");

    // Parse the input into a graph keeping track of vertices and edges.
    let (vertices, edges) = input.lines().fold(
        (HashSet::new(), HashMap::new()),
        |(mut vertices, mut edges), line| {
            let (left, right) = line.split_once("-").unwrap();
            vertices.insert(left);
            vertices.insert(right);
            edges.entry(left).or_insert_with(HashSet::new).insert(right);
            edges.entry(right).or_insert_with(HashSet::new).insert(left);
            (vertices, edges)
        },
    );
    println!("parse: {:?}", now.elapsed());

    // For part 1, simply find all the cliques of size 3 and count the ones that
    // contain a "t" in one of the vertices.
    let now = std::time::Instant::now();
    let cliques  = find_cliques_n(&vertices, &edges, 3);
    let p1 = cliques
        .iter()
        .filter(|clique| clique.iter().find(|v| v.starts_with('t')).is_some())
        .count();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // let now = std::time::Instant::now();
    // let cliques = find_cliques_n(&vertices, &edges, 10);
    // println!("cliques-10: {} ({:?})", cliques.len(), now.elapsed());

    // Use bron_kerbosch to find the largest clique.
    // https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
    let now = std::time::Instant::now();
    let largest_clique = bron_kerbosch(
        &HashSet::new(),
        &mut vertices.iter().cloned().collect(),
        &mut HashSet::new(),
        &edges,
    );

    // For part 2, the solution is to sort the vertices and return them as a
    // comma-separated string.
    let mut largest_clique = largest_clique.iter().collect::<Vec<_>>();
    largest_clique.sort();
    let p2 = largest_clique.iter().join(",");
    println!("p2: {} ({:?})", p2, now.elapsed());
}

fn find_cliques_n<'a>(
    vertices: &HashSet<&'a str>,
    edges: &HashMap<&'a str, HashSet<&'a str>>,
    n: usize,
) -> HashSet<Vec<&'a str>> {
    let mut cliques = HashSet::new();

    // We basically test every vertex for a clique of size k. To do this, we
    // start by adding all of the vertices to a queue with a clique of just
    // itself.
    let mut queue = vertices
        .into_iter()
        .map(|&v| (v, vec![v]))
        .collect::<VecDeque<_>>();
    while let Some((vertex, mut clique)) = queue.pop_front() {
        // If we've found a click of size k, we add it to the set of cliques. We sort
        // the clique to make sure we don't add duplicates.
        if clique.len() == n {
            clique.sort();
            cliques.insert(clique.clone());
            continue;
        }

        // Get a list of neighbors for the current vertex. We only want to
        // consider neighbors that are connected to all of the vertices in the
        // current clique.
        let mut neighbors = edges.get(vertex).unwrap().clone();
        neighbors.retain(|neighbor| {
            clique
                .iter()
                .all(|v| edges.get(v).unwrap().contains(neighbor))
        });

        // Add all of the valid neighbors to the queue with the current clique.
        for neighbor in neighbors {
            let mut new_clique = clique.clone();
            new_clique.push(neighbor);
            queue.push_back((neighbor, new_clique));
        }
    }

    // Return all the cliques we found.
    cliques
}

// Bron-Kerbosch algorithm is a recursive algorithm for finding all maximal cliques in an undirected graph.
// r is the set of vertices in the current clique.
// p is the set of vertices that can be added to the current clique.
// x is the set of vertices that cannot be added to the current clique.
fn bron_kerbosch<'a>(
    r: &HashSet<&'a str>,
    p: &mut HashSet<&'a str>,
    x: &mut HashSet<&'a str>,
    edges: &HashMap<&'a str, HashSet<&'a str>>,
) -> HashSet<&'a str> {
    // If we have checked all our potential vertices and have no excluded
    // vertices, we have found a clique.  We check if the current clique is
    // larger than the largest clique we have found so far and store it.
    if p.is_empty() && x.is_empty() {
        return r.clone();
    }

    // Try adding each vertex in p to the current clique.
    let mut largest_clique = HashSet::new();
    for vertex in p.clone() {
        // Make a new r with the current vertex added.
        let mut r = r.clone();
        r.insert(vertex);

        // Make a new p and x where their vertices are the intersection of the
        // current vertex's neighbors and the current p and x.
        let neighbors = edges.get(vertex).unwrap();
        let mut new_p = p.intersection(neighbors).cloned().collect();
        let mut new_x = x.intersection(neighbors).cloned().collect();
        // Recurse with the new r, p, and x.
        let clique = bron_kerbosch(&r, &mut new_p, &mut new_x, edges);
        if clique.len() > largest_clique.len() {
            largest_clique = clique;
        }

        // Move the current vertex from p to x for the next iteration.
        p.remove(vertex);
        x.insert(vertex);
    }

    // Return the largest clique we found.
    largest_clique
}

#[allow(dead_code)]
fn mermaid(vertices: &HashSet<&str>, edges: &HashMap<&str, HashSet<&str>>) {
    println!("graph TD;");
    for vertex in vertices {
        println!("  {};", vertex);
    }
    for (from, tos) in edges {
        for to in tos {
            println!("  {} --> {};", from, to);
        }
    }
}
