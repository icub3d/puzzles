use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    fs,
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
struct Point {
    x: usize,
    y: usize,
}

impl Ord for Point {
    fn cmp(&self, other: &Self) -> Ordering {
        other.x.cmp(&self.x).then_with(|| self.y.cmp(&other.y))
    }
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: usize,
    point: Point,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.point.cmp(&other.point))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let mut graph = input
        .lines()
        .map(|l| l.chars().map(|c| c as usize).collect::<Vec<usize>>())
        .collect::<Vec<Vec<usize>>>();
    let start = find(&graph, 'S' as usize);
    let end = find(&graph, 'E' as usize);
    graph[start.y][start.x] = 'a' as usize;
    graph[end.y][end.x] = 'z' as usize;

    println!("p1: {:?}", dijkstra(&graph, start, end).unwrap());

    let mut shortest = usize::MAX;
    for (y, r) in graph.iter().enumerate() {
        for (x, c) in r.iter().enumerate() {
            if *c == 'a' as usize {
                shortest =
                    shortest.min(dijkstra(&graph, Point { x, y }, end).unwrap_or(usize::MAX));
            }
        }
    }
    println!("p2: {:?}", shortest);
}

fn valid_moves(graph: &[Vec<usize>], p: &Point) -> Vec<Point> {
    let mut valid = vec![];
    if p.x > 0 && graph[p.y][p.x - 1] <= graph[p.y][p.x] + 1 {
        valid.push(Point { x: p.x - 1, y: p.y });
    }
    if p.x < graph[p.y].len() - 1 && graph[p.y][p.x + 1] <= graph[p.y][p.x] + 1 {
        valid.push(Point { x: p.x + 1, y: p.y });
    }
    if p.y > 0 && graph[p.y - 1][p.x] <= graph[p.y][p.x] + 1 {
        valid.push(Point { x: p.x, y: p.y - 1 });
    }
    if p.y < graph.len() - 1 && graph[p.y + 1][p.x] <= graph[p.y][p.x] + 1 {
        valid.push(Point { x: p.x, y: p.y + 1 });
    }

    valid
}

fn find(graph: &[Vec<usize>], search: usize) -> Point {
    for (y, r) in graph.iter().enumerate() {
        for (x, c) in r.iter().enumerate() {
            if *c == search {
                return Point { x, y };
            }
        }
    }
    Point { x: 0, y: 0 }
}

fn dijkstra(graph: &[Vec<usize>], start: Point, end: Point) -> Option<usize> {
    let mut dist: HashMap<Point, usize> = HashMap::new();
    let mut heap = BinaryHeap::new();

    // Setup our beginning position.
    dist.insert(start, 0);
    heap.push(State {
        cost: 0,
        point: start,
    });
    while let Some(State { cost, point }) = heap.pop() {
        // If we hit the end, we know the shortest distance now.
        if point == end {
            return Some(cost);
        }

        // If we have a distance already and it's smaller, we don't
        // need to keep down this path.
        if cost > *dist.get(&point).unwrap_or(&usize::MAX) {
            continue;
        }

        // Loop through each of this points edges.
        for edge in valid_moves(&graph, &point) {
            let next = State {
                cost: cost + 1,
                point: edge,
            };
            if next.cost < *dist.get(&next.point).unwrap_or(&usize::MAX) {
                heap.push(next);
                dist.insert(next.point, next.cost);
            }
        }
    }
    None
}
