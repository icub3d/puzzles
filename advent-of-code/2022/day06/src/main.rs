use std::{collections::HashSet, fs};

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let chars = lines.chars().collect::<Vec<char>>();
    for (i, w) in chars.windows(4).enumerate() {
        if w[0] != w[1]
            && w[0] != w[2]
            && w[0] != w[3]
            && w[1] != w[2]
            && w[1] != w[3]
            && w[2] != w[3]
        {
            println!("p1: {}", i + 4);
            break;
        }
    }

    for (i, w) in chars.windows(14).enumerate() {
        let s = w.into_iter().cloned().collect::<HashSet<char>>();
        if s.len() == 14 {
            println!("p2: {}", i + 14);
            break;
        }
    }
}
