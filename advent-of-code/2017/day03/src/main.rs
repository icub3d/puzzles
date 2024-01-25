use std::collections::HashMap;

fn p1(w: isize) -> isize {
    let mut i: isize = 1; // iteration
    let mut l: isize = 3; // length of side
    let mut p: isize = 8; // perimiter
    let mut v: isize = 9; // volume

    // Find the completed spiral that contains w.
    while v < w {
        i += 1;
        l += 2;
        p += 8;
        v += p;
    }

    if v == w {
        return i + i;
    }

    let mut cur = v;
    let (mut x, mut y): (isize, isize) = (i, i);

    for (dx, dy) in &[(-1, 0), (0, 1), (1, 0), (0, -1)] {
        if w > cur - l {
            // We are inside this side.
            x += dx * (cur - w);
            y += dy * (cur - w);
            return x.abs() + y.abs();
        } else {
            x += dx * l;
            y += dy * l;
            cur -= l;
        }
    }

    panic!("you dun goofed");
}

fn sum_surrounding(grid: &HashMap<(isize, isize), isize>, x: isize, y: isize) -> isize {
    let mut sum = 0;
    for dx in -1..=1 {
        for dy in -1..=1 {
            if dx == 0 && dy == 0 {
                continue;
            }
            sum += grid.get(&(x + dx, y + dy)).unwrap_or(&0);
        }
    }
    sum
}

fn p2(input: isize) -> isize {
    let mut known = HashMap::new();
    known.insert((0, 0), 1);
    let (mut x, mut y) = (1, 0);
    let mut l = 3;
    let deltas = [(0, 1, 2), (-1, 0, 1), (0, -1, 1), (1, 0, 0)];

    loop {
        for (dx, dy, offset) in deltas.iter() {
            for _ in 0..l - offset {
                let sum = sum_surrounding(&known, x, y);
                if sum > input {
                    return sum;
                }
                known.insert((x, y), sum);
                x += dx;
                y += dy;
            }
        }
        l += 2;
    }
}

fn main() {
    let input = 368078;
    let now = std::time::Instant::now();
    println!("p1: {} ({:?})", p1(input), now.elapsed());
    let now = std::time::Instant::now();
    println!("p2: {} ({:?})", p2(input), now.elapsed());
}
