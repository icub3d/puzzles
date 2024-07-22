use std::collections::VecDeque;

use md5::{Digest, Md5};

type Encoder = fn(&str, usize) -> String;

fn encode(salt: &str, i: usize) -> String {
    let data = format!("{}{}", salt, i);
    let mut hasher = Md5::new();
    hasher.update(data.as_bytes());
    format!("{:x}", hasher.finalize())
}

fn encode2016(salt: &str, i: usize) -> String {
    let mut hash = encode(salt, i);
    for _ in 0..2016 {
        let mut hasher = Md5::new();
        hasher.update(hash.as_bytes());
        hash = format!("{:x}", hasher.finalize());
    }
    hash
}

fn main() {
    // p1: 15035 (123.248738ms)
    // p2: 19968 (6.912950947s)
    let salt = "ihaygndm";
    // let salt = "abc";

    let now = std::time::Instant::now();
    println!("p1: {} ({:?})", run(salt, encode), now.elapsed());
    let now = std::time::Instant::now();
    println!("p2: {} ({:?})", run(salt, encode2016), now.elapsed());
}

fn run(salt: &str, e: Encoder) -> usize {
    let mut hashes = (0..=1000).map(|i| e(salt, i)).collect::<VecDeque<_>>();
    let mut i = 0;
    let mut found = 0;
    loop {
        let hash = &hashes[0].chars().collect::<Vec<_>>()[..];
        if let Some(cc) = hash.windows(3).find(|w| w[0] == w[1] && w[1] == w[2]) {
            let c = cc[0];
            if (1..1001).any(|j| {
                hashes[j].chars().collect::<Vec<_>>()[..]
                    .windows(5)
                    .any(|w| w[0] == c && w[1] == c && w[2] == c && w[3] == c && w[4] == c)
            }) {
                found += 1;
                if found == 64 {
                    return i;
                }
            }
        }
        i += 1;
        hashes.pop_front();
        hashes.push_back(e(salt, i + 1000));
    }
}
