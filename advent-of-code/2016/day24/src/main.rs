use std::collections::HashSet;

use pathfinding::prelude::dijkstra;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn neighbors(&self) -> Vec<Point> {
        vec![
            Point {
                x: self.x + 1,
                y: self.y,
            },
            Point {
                x: self.x - 1,
                y: self.y,
            },
            Point {
                x: self.x,
                y: self.y + 1,
            },
            Point {
                x: self.x,
                y: self.y - 1,
            },
        ]
    }
}

fn main() {
    let input = include_str!("../input");
    let (poi, graph) = input.lines().enumerate().fold(
        (vec![], HashSet::new()),
        |(mut poi, mut graph), (y, line)| {
            line.chars().enumerate().for_each(|(x, c)| {
                if c == '.' || c.is_ascii_digit() {
                    let p = Point { x, y };
                    if c.is_ascii_digit() {
                        poi.push((c.to_digit(10).unwrap(), p));
                    }
                    graph.insert(p);
                }
            });
            (poi, graph)
        },
    );

    let mut dists = vec![vec![0; poi.len()]; poi.len()];
    for (a, p1) in &poi {
        for (b, p2) in &poi {
            if a == b {
                continue;
            }
            let successors = |p: &Point| -> Vec<(Point, usize)> {
                p.neighbors()
                    .into_iter()
                    .filter(|p| graph.contains(p))
                    .map(|p| (p, 1))
                    .collect()
            };
            let success = |p: &Point| -> bool { p == p2 };
            if let Some((_, cost)) = dijkstra(p1, successors, success) {
                dists[*a as usize][*b as usize] = cost;
            }
        }
    }
    println!("{:?}", dists);

    let mut shortest = usize::MAX;
    let mut visited = vec![0];
    p1(&dists, &mut visited, &mut shortest, 0);
    println!("p1: {}", shortest);

    let mut shortest = usize::MAX;
    let mut visited = vec![0];
    p2(&dists, &mut visited, &mut shortest, 0);
    println!("p2: {}", shortest);
}

fn p1(dists: &[Vec<usize>], visited: &mut Vec<usize>, shortest: &mut usize, cur_dist: usize) {
    if visited.len() == dists.len() {
        *shortest = cur_dist.min(*shortest);
        return;
    }
    for i in 0..dists.len() {
        if visited.contains(&i) {
            continue;
        }
        visited.push(i);
        p1(
            dists,
            visited,
            shortest,
            cur_dist + dists[visited[visited.len() - 2]][i],
        );
        visited.pop();
    }
}

fn p2(dists: &[Vec<usize>], visited: &mut Vec<usize>, shortest: &mut usize, cur_dist: usize) {
    if visited.len() == dists.len() {
        let cur_dist = cur_dist + dists[visited[visited.len() - 1]][0];
        *shortest = cur_dist.min(*shortest);
        return;
    }
    for i in 0..dists.len() {
        if visited.contains(&i) {
            continue;
        }
        visited.push(i);
        p2(
            dists,
            visited,
            shortest,
            cur_dist + dists[visited[visited.len() - 2]][i],
        );
        visited.pop();
    }
}
