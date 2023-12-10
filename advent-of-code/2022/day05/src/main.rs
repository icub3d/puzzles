use std::fs;

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let mut lines = lines.lines().into_iter();

    // Create the map.
    let mut p1 = vec![vec![]; 9];
    loop {
        let line = lines.next().unwrap();
        if line == "" {
            break;
        }
        for (i, c) in line.chars().enumerate() {
            if c != ' ' {
                p1[i].push(c);
            }
        }
    }

    // Reverse all of the vectors.
    for i in 0..p1.len() {
        p1[i] = p1[i].iter().rev().cloned().collect();
    }
    let mut p2 = p1.clone();

    // Perform all the operations.
    loop {
        let line = match lines.next() {
            Some(line) => line,
            None => break,
        };
        let parts = line.split(' ').collect::<Vec<&str>>();
        let count = parts[1].parse::<usize>().unwrap();
        let from = parts[3].parse::<usize>().unwrap() - 1;
        let to = parts[5].parse::<usize>().unwrap() - 1;

        let mut p2s = vec![];
        for _ in 0..count {
            let v = p1[from].pop().unwrap();
            p1[to].push(v);
            let v = p2[from].pop().unwrap();
            p2s.push(v);
        }
        p2s.reverse();
        p2[to].append(&mut p2s);
    }

    // Print the result.
    print!("p1: ");
    for i in 0..p1.len() {
        print!("{}", p1[i][p1[i].len() - 1]);
    }
    print!("\np1: ");
    for i in 0..p2.len() {
        print!("{}", p2[i][p2[i].len() - 1]);
    }
    print!("\n");
}
