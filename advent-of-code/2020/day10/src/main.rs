use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use std::collections::HashMap;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let mut aa: Vec<usize> = buf
        .lines()
        .map(|l| (l.unwrap().parse::<usize>().unwrap()))
        .collect();
    aa.sort();
    aa.insert(0, 0);
    aa.push(aa[aa.len() - 1] + 3);

    let mut diffs: [usize; 3] = [0; 3];
    for i in 1..aa.len() {
        if aa[i - 1] == aa[i] {
            continue;
        }
        let diff = aa[i] - aa[i - 1];
        if diff > 3 {
            println!("too far: {} {} {}", i, aa[i], aa[i - 1]);
            break;
        }
        diffs[diff - 1] += 1;
    }
    println!("{:?} {}", diffs, diffs[0] * diffs[2]);

    let last = aa.last().unwrap();
    let mut hits: HashMap<usize, usize> = HashMap::new();
    hits.insert(aa[0], 1);
    for a in aa.iter() {
        let cur = *hits.get(a).unwrap();
        for s in 1..4 {
            let next = a + s;
            if aa.contains(&next) {
                *hits.entry(next).or_insert(0) += cur;
            }
        }
    }
    println!("combinations {}", *hits.get(&last).unwrap());
}
