use std::fs;

fn main() {
    let depths = fs::read_to_string("input").unwrap();
    let depths: Vec<usize> = depths
        .lines()
        .map(|line| line.parse::<usize>().unwrap())
        .collect();

    let mut last = depths[0];
    let mut increasing = 0;
    for depth in depths.iter() {
        if last < *depth {
            increasing += 1;
        }
        last = *depth;
    }
    println!("{}", increasing);

    last = depths[0] + depths[1] + depths[2];
    increasing = 0;
    for i in 1..depths.len() - 1 {
        let sum = depths[i - 1] + depths[i] + depths[i + 1];
        if last < sum {
            increasing += 1;
        }
        last = sum;
    }
    println!("{}", increasing);
}
