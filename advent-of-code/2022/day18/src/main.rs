use std::{collections::HashSet, fs, ops::Add};

const DELTAS: [Point; 6] = [
    Point { x: -1, y: 0, z: 0 },
    Point { x: 1, y: 0, z: 0 },
    Point { x: 0, y: -1, z: 0 },
    Point { x: 0, y: 1, z: 0 },
    Point { x: 0, y: 0, z: -1 },
    Point { x: 0, y: 0, z: 1 },
];

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
}

impl Point {
    fn new(s: &str) -> Self {
        let parts = s.split(',').collect::<Vec<&str>>();
        Self {
            x: parts[0].parse().unwrap(),
            y: parts[1].parse().unwrap(),
            z: parts[2].parse().unwrap(),
        }
    }

    fn adjacent(&self) -> HashSet<Point> {
        DELTAS.iter().map(|p| *p + *self).collect()
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

fn area(pp: &HashSet<Point>) -> usize {
    let mut sides = pp.len() * 6;
    for p in pp.iter() {
        for a in p.adjacent() {
            if pp.contains(&a) {
                sides -= 1;
            }
        }
    }
    sides
}

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let points = lines.lines().map(Point::new).collect::<HashSet<Point>>();
    let mut aa = area(&points);
    println!("p1: {aa}");

    let min = Point {
        x: points.iter().map(|p| p.x).min().unwrap(),
        y: points.iter().map(|p| p.y).min().unwrap(),
        z: points.iter().map(|p| p.z).min().unwrap(),
    };
    let max = Point {
        x: points.iter().map(|p| p.x).max().unwrap(),
        y: points.iter().map(|p| p.y).max().unwrap(),
        z: points.iter().map(|p| p.z).max().unwrap(),
    };

    // For each point that isn't in the points set (this means it's air). We try to mark all surrounding points. If they touch an edge of the graph, then it's outside. If it doesn't touch an edge of the graph, then it's inside and we should subtract it's surface area from the surface area of the grid.
    let mut seen: HashSet<Point> = HashSet::new();
    for x in min.x..=max.x {
        for y in min.y..=max.y {
            for z in min.z..=max.z {
                let p = Point { x, y, z };
                if points.contains(&p) || seen.contains(&p) {
                    continue;
                }

                // Try to find all adjacent to this one.
                let mut near = HashSet::new();
                fill(&points, &min, &max, &mut near, p);

                // If this diesn't extend outside, we found an area we should subtract.
                if near
                    .iter()
                    .filter(|p| {
                        p.x == min.x
                            || p.x == max.x
                            || p.y == min.y
                            || p.y == max.y
                            || p.z == min.z
                            || p.z == max.z
                    })
                    .count()
                    == 0
                {
                    aa -= area(&near);
                }

                // Mark them all as seen regardless so we don't have to check them again.
                seen.extend(&near);
            }
        }
    }
    println!("p2: {aa}");
}

fn fill(points: &HashSet<Point>, min: &Point, max: &Point, near: &mut HashSet<Point>, cur: Point) {
    near.insert(cur.clone());
    for adj in cur.adjacent() {
        if points.contains(&adj)
            || near.contains(&adj)
            || adj.x < min.x
            || adj.x > max.x
            || adj.y < min.y
            || adj.y > max.y
            || adj.z < min.z
            || adj.z > max.z
        {
            continue;
        }
        fill(points, min, max, near, adj);
    }
}
