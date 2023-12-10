use rustc_hash::FxHashMap;
use std::fs;
fn main() {
    let init: Vec<usize> = fs::read_to_string("input")
        .unwrap()
        .split(',')
        .map(|s| {
            println!("{}", s);
            s.parse::<usize>().unwrap()
        })
        .collect();
    let mut numbers: Vec<usize> = Vec::with_capacity(30_000_000);
    let mut last: FxHashMap<usize, usize> = FxHashMap::default();
    for (i, &v) in init.iter().enumerate() {
        numbers.push(v);
        if i < init.len() - 1 {
            last.insert(v, i);
        }
    }

    while numbers.len() < 30_000_000 {
        match last.insert(*numbers.last().unwrap(), numbers.len() - 1) {
            None => numbers.push(0),
            Some(n) => numbers.push(numbers.len() - n - 1),
        }
    }
    println!("{} {}", numbers[2019], numbers[29_999_999]);
}
