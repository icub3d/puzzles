use std::collections::HashMap;

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let lines = input.lines().collect::<Vec<_>>();

    let mut freq = vec![HashMap::new(); lines[0].len()];
    for line in lines {
        for (i, c) in line.chars().enumerate() {
            *freq[i].entry(c).or_insert(0) += 1;
        }
    }

    let mut msg = String::new();
    for f in &freq {
        if let Some((k, _)) = f.iter().max_by_key(|&(_, v)| v) {
            msg.push(*k);
        }
    }
    println!("p1: {}", msg);

    let mut msg = String::new();
    for f in freq {
        if let Some((k, _)) = f.iter().min_by_key(|&(_, v)| v) {
            msg.push(*k);
        }
    }
    println!("p2: {}", msg);
}
