use std::collections::{HashMap, HashSet};

fn main() {
    let input = include_str!("../input");
    let lines = input.lines().collect::<Vec<_>>();

    let (_, _, _, first) = lines[0].split(',').fold(
        (0, 0, 0, HashMap::new()),
        |(mut x, mut y, mut steps, mut grid), step| {
            let (dir, dist) = step.split_at(1);
            let dist = dist.parse::<i32>().unwrap();
            let (dx, dy) = match dir {
                "U" => (0, 1),
                "D" => (0, -1),
                "L" => (-1, 0),
                "R" => (1, 0),
                _ => panic!("Unknown direction"),
            };
            for _ in 0..dist {
                x += dx;
                y += dy;
                steps += 1;
                grid.insert((x, y), steps);
            }
            (x, y, steps, grid)
        },
    );

    let (_, _, _, second): (isize, isize, isize, HashMap<(isize, isize), isize>) =
        lines[1].split(',').fold(
            (0, 0, 0, HashMap::new()),
            |(mut x, mut y, mut steps, mut grid), step| {
                let (dir, dist) = step.split_at(1);
                let dist = dist.parse::<i32>().unwrap();
                let (dx, dy) = match dir {
                    "U" => (0, 1),
                    "D" => (0, -1),
                    "L" => (-1, 0),
                    "R" => (1, 0),
                    _ => panic!("Unknown direction"),
                };
                for _ in 0..dist {
                    x += dx;
                    y += dy;
                    steps += 1;
                    grid.insert((x, y), steps);
                }
                (x, y, steps, grid)
            },
        );

    let first_keys = first.keys().collect::<HashSet<_>>();
    let second_keys = second.keys().collect::<HashSet<_>>();
    let p1: isize = first_keys
        .intersection(&second_keys)
        .map(|(x, y)| x.abs() + y.abs())
        .filter(|&x| x != 0)
        .min()
        .unwrap();
    println!("p1: {}", p1);

    let p2: isize = first_keys
        .intersection(&second_keys)
        .map(|k| first[k] + second[k])
        .filter(|&x| x != 0)
        .min()
        .unwrap();
    println!("p2: {}", p2);
}
