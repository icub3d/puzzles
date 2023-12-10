use std::{collections::HashSet, fs};

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let lines = lines.lines().collect::<Vec<&str>>();

    let (mut hx, mut hy) = (0, 0);
    let (mut tx, mut ty) = (0, 0);
    let mut visited: HashSet<(isize, isize)> = HashSet::new();
    visited.insert((tx, ty));

    for line in lines.clone() {
        let parts = line.split(' ').collect::<Vec<&str>>();
        let n = parts[1].parse::<usize>().unwrap();
        let (dx, dy) = match parts[0] {
            "U" => (0, 1),
            "D" => (0, -1),
            "R" => (1, 0),
            _ => (-1, 0),
        };

        for _ in 0..n {
            hx += dx;
            hy += dy;
            (tx, ty) = move_tail(tx, ty, hx, hy);
            visited.insert((tx, ty));
        }
    }

    println!("p1: {}", visited.len());

    let mut knots = vec![(0, 0); 10];
    let mut visited: HashSet<(isize, isize)> = HashSet::new();
    visited.insert((0, 0));
    for line in lines {
        let parts = line.split(' ').collect::<Vec<&str>>();
        let n = parts[1].parse::<usize>().unwrap();
        let (dx, dy) = match parts[0] {
            "U" => (0, 1),
            "D" => (0, -1),
            "R" => (1, 0),
            _ => (-1, 0),
        };
        for _ in 0..n {
            knots[0].0 += dx;
            knots[0].1 += dy;
            for i in 1..knots.len() {
                knots[i] = move_tail(knots[i].0, knots[i].1, knots[i - 1].0, knots[i - 1].1);
            }
            visited.insert(knots[knots.len() - 1]);
        }
    }
    println!("p2: {}", visited.len());
}

fn move_tail(tx: isize, ty: isize, hx: isize, hy: isize) -> (isize, isize) {
    if hx == tx && hy == ty {
        // Same position
        (tx, ty)
    } else if tx == hx {
        // horizontal, just bring closer
        match hy - ty {
            2 => (tx, ty + 1),
            -2 => (tx, ty - 1),
            _ => (tx, ty),
        }
    } else if ty == hy {
        // vertical, just bring closer
        match hx - tx {
            2 => (tx + 1, ty),
            -2 => (tx - 1, ty),
            _ => (tx, ty),
        }
    } else if (hx - tx).abs() > 1 || (hy - ty).abs() > 1 {
        // diagonal and greater than 1.
        if hy > ty && hx > tx {
            (tx + 1, ty + 1)
        } else if hy > ty && hx < tx {
            (tx - 1, ty + 1)
        } else if hy < ty && hx < tx {
            (tx - 1, ty - 1)
        } else {
            (tx + 1, ty - 1)
        }
    } else {
        (tx, ty)
    }
}
