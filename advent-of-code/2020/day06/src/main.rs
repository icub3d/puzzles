use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::FromIterator;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    day6(&aa);
    day6_2(&aa);
}

fn day6(aa: &[String]) {
    let mut total = 0;
    let mut all: HashSet<char> = HashSet::new();

    for a in aa.iter() {
        if a == "" {
            total += all.len();
            all = HashSet::new();
            continue;
        }
        for c in a.chars() {
            all.insert(c);
        }
    }
    total += all.len();

    println!("total: {}", total)
}

fn day6_2(aa: &[String]) {
    let mut total = 0;
    let mut all: HashSet<char> = HashSet::new();
    let mut none = false;
    for a in aa.iter() {
        if a == "" {
            none = false;
            total += all.len();
            all = HashSet::new();
            continue;
        }
        if none {
            continue;
        }
        if all.len() == 0 {
            all = HashSet::from_iter(a.chars());
        } else {
            let cur = HashSet::from_iter(a.chars());
            all = all.intersection(&cur).map(|x| *x).collect();
            if all.len() == 0 {
                none = true;
            }
        }
    }
    total += all.len();

    println!("total: {}", total)
}
