use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fs;

lazy_static! {
    static ref VALID_VALUES: Vec<char> = vec!['A', 'B', 'C', 'D', '.'];
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Color {
    Amber,
    Bronze,
    Copper,
    Desert,
}

impl Color {
    fn valid_destination(&self, p: &Point) -> bool {
        match *self {
            Color::Amber => p.x == 3 && p.y > 1,
            Color::Bronze => p.x == 5 && p.y > 1,
            Color::Copper => p.x == 7 && p.y > 1,
            Color::Desert => p.x == 9 && p.y > 1,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Node {
    Empty,
    Unstoppable,
    Occupied(Color),
}

#[derive(Hash, Debug, Eq, PartialEq, Clone)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn hallway(&self) -> bool {
        self.x == 1
    }

    fn surrounding(&self, max_x: usize, max_y: usize) -> Vec<Point> {
        let mut points: Vec<Point> = vec![];
        if self.x > 0 {
            points.push(Point {
                x: self.x - 1,
                y: self.y,
            });
        }
        if self.x < max_x {
            points.push(Point {
                x: self.x + 1,
                y: self.y,
            });
        }
        if self.y > 0 {
            points.push(Point {
                x: self.x,
                y: self.y - 1,
            });
        }
        if self.y < max_y {
            points.push(Point {
                x: self.x,
                y: self.y + 1,
            });
        }
        points
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Graph {
    nodes: HashMap<Point, Node>,
    edges: HashMap<Point, Vec<Point>>,
}

impl Graph {
    fn new(file: &str) -> Graph {
        let input = fs::read_to_string(file).unwrap();
        let text: Vec<Vec<char>> = input.lines().map(|v| v.chars().collect()).collect();

        // Create our nodes and their current state and our edges.
        let mut nodes: HashMap<Point, Node> = HashMap::new();
        let mut edges: HashMap<Point, Vec<Point>> = HashMap::new();
        for (x, list) in text.iter().enumerate() {
            for (y, c) in list.iter().enumerate() {
                match c {
                    'A' => nodes.insert(Point { x, y }, Node::Occupied(Color::Amber)),
                    'B' => nodes.insert(Point { x, y }, Node::Occupied(Color::Bronze)),
                    'C' => nodes.insert(Point { x, y }, Node::Occupied(Color::Copper)),
                    'D' => nodes.insert(Point { x, y }, Node::Occupied(Color::Desert)),
                    '.' => nodes.insert(Point { x, y }, Node::Empty),
                    _ => None,
                };
                if VALID_VALUES.contains(c) {
                    let p = Point { x, y };
                    edges.insert(
                        p.clone(),
                        p.surrounding(text.len() - 1, list.len() - 1)
                            .into_iter()
                            .filter(|p| VALID_VALUES.contains(&text[p.x][p.y]))
                            .collect(),
                    );
                }
            }
        }

        // Find the unstoppable nodes.
        for (p, e) in edges.iter() {
            match nodes.get(p).unwrap() {
                Node::Occupied(_) => {
                    // If we find an occupied spot, on the initial graph,
                    // any empty nodes are unstoppable.
                    for p in e.iter() {
                        match nodes.get(p).unwrap() {
                            Node::Empty => {
                                nodes.insert(p.clone(), Node::Unstoppable);
                            }
                            _ => (),
                        };
                    }
                }
                _ => (),
            };
        }
        Graph { nodes, edges }
    }

    fn final_destinations(&self, c: Color) -> Vec<Point> {
        self.edges
            .keys()
            .cloned()
            .filter(|p| c.valid_destination(p))
            .collect()
    }
}

fn main() {
    let mut g = Graph::new("input");
    dbg!(g);
}
