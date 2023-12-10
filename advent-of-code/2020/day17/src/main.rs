use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use counter::Counter;

use std::collections::HashSet;

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
    w: isize,
}

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    // Get the cubes.
    let mut cubes: HashSet<Point> = HashSet::new();
    for (x, line) in aa.iter().enumerate() {
        for (y, c) in line.chars().enumerate() {
            if c == '#' {
                cubes.insert(Point {
                    x: x as isize,
                    y: y as isize,
                    z: 0 as isize,
                    w: 0 as isize,
                });
            }
        }
    }

    // Part 1
    let p2 = cubes.clone();
    for _ in 0..6 {
        // Get a count of all of the neighbors of all of the points in
        // the set (the ones turned on).
        let counter = cubes
            .iter()
            // only working in 3-d here.
            .flat_map(|p| {
                neighbors(p)
                    .iter()
                    .filter(|p| p.w == 0)
                    .map(|p| *p)
                    .collect::<Vec<Point>>()
            })
            .collect::<Counter<_>>();

        // Determine which points are should stay on.
        let mut on: HashSet<Point> = cubes
            .iter()
            .filter(|p| {
                let n = counter[p];
                n >= 2 && n <= 3
            })
            .map(|p| *p)
            .collect();

        // Determine which points should switch.
        let switch = counter
            .iter()
            .filter(|(p, n)| **n == 3 && !cubes.contains(p))
            .map(|(p, _)| *p);

        // merge the two and update it.
        on.extend(switch);
        cubes = on;
    }
    println!("{:?}", cubes.len());

    // Part 2 is like part one but in 4d, so we won't filter the counters.
    let mut cubes = p2;
    for _ in 0..6 {
        // Get a count of all of the neighbors of all of the points in
        // the set (the ones turned on).
        let counter = cubes
            .iter()
            .flat_map(|p| neighbors(p))
            .collect::<Counter<_>>();

        // Determine which points are should stay on.
        let mut on: HashSet<Point> = cubes
            .iter()
            .filter(|p| {
                let n = counter[p];
                n >= 2 && n <= 3
            })
            .map(|p| *p)
            .collect();

        // Determine which points should switch.
        let switch = counter
            .iter()
            .filter(|(p, n)| **n == 3 && !cubes.contains(p))
            .map(|(p, _)| *p);

        // merge the two and update it.
        on.extend(switch);
        cubes = on;
    }
    println!("{:?}", cubes.len());
}

fn neighbors(p: &Point) -> Vec<Point> {
    let mut v = vec![];
    for dx in -1..=1 {
        for dy in -1..=1 {
            for dz in -1..=1 {
                for dw in -1..=1 {
                    if dx == 0 && dy == 0 && dz == 0 && dw == 0 {
                        continue;
                    }
                    v.push(Point {
                        x: p.x + dx,
                        y: p.y + dy,
                        z: p.z + dz,
                        w: p.w + dw,
                    });
                }
            }
        }
    }
    v
}
