use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

#[derive(Clone, Debug)]
struct Node<'a> {
	id: &'a str,
	edges: HashSet<&'a str>,
	small: bool,
}

impl<'a> Node<'a> {
	fn new(id: &'a str) -> Self {
		Self {
			id,
			edges: HashSet::new(),
			small: id.chars().all(char::is_lowercase),
		}
	}

	// We can solve this recursively by just tracking the small nodes
	// that we've cisited.
	fn paths(
		&self,
		allow_two: bool,
		graph: &HashMap<&'a str, Node<'a>>,
		visited: &mut HashMap<&'a str, isize>,
	) -> usize {
		// If we hit the end, we've found a path.
		if self.id == "end" {
			return 1;
		}

		let mut sum = 0;
		// Add ourselves to the visited list so we know we've been here.
		if self.small {
			*visited.entry(self.id).or_insert(0) += 1;
		}
		for node in self.edges.iter() {
			let node = graph.get(node).unwrap();

			if node.id == "start" {
				// We don't want to visit start again.
				continue;
			} else if node.small {
				let two_visited = visited.iter().map(|(_, v)| v).filter(|v| v > &&1).count() > 0;
				let this_visited = *visited.get(node.id).unwrap_or(&0);

				// If we aren't allowing two, then we can't have
				// visited this node before.  If we are allowing two,
				// but two are visited already and this one is
				// visited, we can't use it.
				if (!allow_two || two_visited) && this_visited > 0 {
					continue;
				}
			}

			sum += graph[node.id].paths(allow_two, graph, visited);
		}
		// Remove ourselves from the visited list.
		if self.small {
			*visited.entry(self.id).or_insert(0) -= 1;
		}
		sum
	}
}

fn main() {
	let lines = fs::read_to_string("input").unwrap();
	let mut graph = HashMap::new();
	for (l, r) in lines.lines().map(|l| {
		let s = l.split('-').collect::<Vec<&str>>();
		(s[0], s[1])
	}) {
		graph
			.entry(l)
			.or_insert_with(|| Node::new(l))
			.edges
			.insert(r);
		graph
			.entry(r)
			.or_insert_with(|| Node::new(r))
			.edges
			.insert(l);
	}

	// We can simply get paths recursively, we just need to track
	// which ones were visited. We use the same function but toggle
	// for allowing two for both parts.
	let mut visited: HashMap<&str, isize> = HashMap::new();
	let start = graph.get("start").unwrap();
	println!(
		"total paths: {:?}",
		start.paths(false, &graph, &mut visited)
	);
	println!(
		"total paths (allow two): {:?}",
		start.paths(true, &graph, &mut visited)
	);
}
