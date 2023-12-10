use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    // Gather rules
    let mut rules: HashMap<String, HashSet<usize>> = HashMap::new();
    let mut all: HashSet<usize> = HashSet::new();
    let mut cur = 0;
    for i in 0..aa.len() {
        let a = &aa[i];
        if a == "" {
            cur = i + 2;
            break;
        }
        let parts: Vec<&str> = a.split(":").collect();
        let values: Vec<&str> = parts[1].trim().split(" ").collect();
        let mut valid: HashSet<usize> = HashSet::new();
        for v in values.iter() {
            if *v == "or" {
                continue;
            }
            let nums: Vec<usize> = v.split("-").map(|x| x.parse::<usize>().unwrap()).collect();
            for j in nums[0]..=nums[1] {
                valid.insert(j);
                all.insert(j);
            }
        }
        rules.insert(parts[0].to_string(), valid);
    }

    // Get mine
    let mine: Vec<usize> = aa[cur]
        .split(",")
        .map(|x| x.parse::<usize>().unwrap())
        .collect();
    cur += 3;

    // find valid and track invalid.
    let mut sum = 0;
    let mut combined: Vec<HashSet<usize>> = vec![HashSet::new(); mine.len()];
    for a in aa[cur..].iter() {
        let nums: Vec<usize> = a.split(",").map(|x| x.parse::<usize>().unwrap()).collect();
        let mut v = true;
        for num in nums.iter() {
            if !all.contains(&num) {
                v = false;
                sum += num;
            }
        }
        if v {
            for (i, num) in nums.iter().enumerate() {
                combined[i].insert(*num);
            }
        }
    }
    println!("invalid sum: {}", sum);

    // Find candidates for each rule.
    let mut matches: HashMap<String, HashSet<usize>> = rules
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                combined
                    .iter()
                    .enumerate()
                    .filter(|(_, set)| set.is_subset(&v))
                    .map(|(i, _)| i)
                    .collect::<HashSet<usize>>(),
            )
        })
        .collect();

    // Risky, we assume once we find a single value and remove it from
    // the rest, it will produce another single value.
    while !all_one(&matches) {
        let (k, v) = find_one(&matches);
        let mut mm: HashMap<String, HashSet<usize>> = HashMap::new();
        mm.insert(k.to_string(), v.clone());
        for (kk, vv) in matches.iter() {
            if k == *kk {
                continue;
            }
            let mut nvv = vv.clone();
            nvv.remove(v.iter().last().unwrap());
            mm.insert(kk.to_string(), nvv);
        }
        matches = mm;
    }

    // find all departures and multiply
    let mul = matches
        .iter()
        .filter(|(k, _)| k.starts_with("departure"))
        .fold(1, |acc, (_, v)| acc * mine[*v.iter().last().unwrap()]);
    println!("departure mul: {}", mul);
}

fn find_one(matches: &HashMap<String, HashSet<usize>>) -> (String, HashSet<usize>) {
    for (k, v) in matches.iter() {
        if v.len() == 1 {
            return (k.clone(), v.clone());
        }
    }
    return ("".to_string(), HashSet::new());
}

fn all_one(matches: &HashMap<String, HashSet<usize>>) -> bool {
    for (_, v) in matches.iter() {
        if v.len() > 1 {
            return false;
        }
    }
    true
}
