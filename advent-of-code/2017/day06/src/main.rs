use std::collections::HashSet;

fn main() {
    let input = include_str!("../input");
    let banks = input
        .split_whitespace()
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<u32>>();

    let mut seen = HashSet::new();
    let (cycles, want) = run(&banks, |b| !seen.insert(b.clone()));
    println!("p1: {}", cycles);

    let mut first = true;
    let (cycles, _) = run(&want, |b| {
        if first {
            first = false;
            return false;
        }
        b == &want
    });
    println!("p2: {}", cycles);
}

fn run(banks: &[u32], mut stop: impl FnMut(&Vec<u32>) -> bool) -> (u32, Vec<u32>) {
    let mut banks = banks.to_vec();
    let mut cycles = 0;
    loop {
        let mut max = 0;
        let mut max_index = 0;
        for (i, &x) in banks.iter().enumerate() {
            if x > max {
                max = x;
                max_index = i;
            }
        }
        let mut blocks = banks[max_index];
        banks[max_index] = 0;
        let mut index = max_index;
        while blocks > 0 {
            index = (index + 1) % banks.len();
            banks[index] += 1;
            blocks -= 1;
        }
        cycles += 1;
        if stop(&banks) {
            break;
        }
    }
    (cycles, banks)
}
