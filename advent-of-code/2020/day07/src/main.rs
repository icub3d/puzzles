use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn add(
    edges: &mut HashMap<String, Vec<(usize, String)>>,
    key: String,
    count: usize,
    value: String,
) {
    if edges.contains_key(&key) {
        edges.get_mut(&key).unwrap().push((count, value));
    } else {
        edges.insert(key, vec![(count, value)]);
    }
}

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    let mut edges: HashMap<String, Vec<(usize, String)>> = HashMap::new();
    for a in aa.iter() {
        let contains: Vec<&str> = a.split(" contain ").collect();
        let left = contains[0].trim_end_matches(" bags");

        let parts: Vec<&str> = contains[1].split(",").collect();
        for p in parts.iter() {
            if *p == "no other bags." {
                add(&mut edges, left.to_string(), 0, "none".to_string());
                continue;
            }
            let p = p
                .trim()
                .trim_end_matches(" bag")
                .trim_end_matches(" bag.")
                .trim_end_matches(" bags")
                .trim_end_matches(" bags.");
            let p: Vec<&str> = p.split(" ").collect();
            let num: usize = p[0].parse().unwrap();
            let p = p[1..].join(" ");
            add(&mut edges, left.to_string(), num, p);
        }
    }

    let mut descendants: HashSet<String> = HashSet::new();
    get_descendants(&mut descendants, &edges, "shiny gold".to_string());
    println!("{}", descendants.len());

    println!("{}", how_many(&edges, 1, "shiny gold".to_string()) - 1);
}

fn how_many(edges: &HashMap<String, Vec<(usize, String)>>, count: usize, color: String) -> usize {
    if color == "none".to_string() {
        return 0;
    }
    let mut total = count;
    for bag in edges.get(&color).unwrap().iter() {
        total += count * how_many(edges, bag.0, bag.1.to_string());
    }
    total
}

fn get_descendants(
    mut descendants: &mut HashSet<String>,
    edges: &HashMap<String, Vec<(usize, String)>>,
    cur: String,
) {
    for (e, v) in edges.iter() {
        if v.iter().any(|i| *i.1 == cur) {
            descendants.insert(e.to_string());
            get_descendants(&mut descendants, edges, e.to_string());
        }
    }
}
