use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let input = File::open("input").unwrap();
    let input = BufReader::new(input);

    let mut all = vec![];
    let mut cur = 0;
    for line in input.lines() {
        let line = line.unwrap();
        if line == "" {
            all.push(cur);
            cur = 0;
            continue;
        }
        cur += line.parse::<i32>().unwrap();
    }

    all.sort_by(|a, b| b.cmp(a));

    println!("p1: {}", all[0]);
    println!("p2: {}", all[0] + all[1] + all[2]);
}
