use std::{collections::HashMap, fs, path::PathBuf};

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let lines = lines.lines();

    let mut path = PathBuf::from("/");
    let mut sizes: HashMap<String, usize> = HashMap::new();
    // Originally used state but there are really only two, so we can just use an if/else here.
    for line in lines {
        let parts = line.split(' ').collect::<Vec<&str>>();
        if parts[0] == "$" {
            match parts[1] {
                "cd" => {
                    match parts[2] {
                        "/" => path = PathBuf::from("/"),
                        ".." => {
                            path.pop();
                            ()
                        }
                        _ => path.push(parts[2]),
                    };
                }
                _ => (),
            };
        } else {
            // in listing
            match parts[0] {
                "dir" => (),
                _ => {
                    let size = parts[0].parse::<usize>().unwrap();
                    // Directories could have the same name in other parts of the file structure.
                    let mut cur = path.clone();
                    loop {
                        *sizes.entry(cur.to_str().unwrap().into()).or_insert(0) += size;
                        match cur.pop() {
                            false => break,
                            true => (),
                        };
                    }
                }
            }
        }
    }

    // Loop through the directories and find all that are at most 100_000.
    let mut p1 = 0;
    for (_, &v) in sizes.iter() {
        if v <= 100_000 {
            p1 += v;
        }
    }
    println!("p1: {p1}");

    // Figure how much we need.
    let root = sizes.get("/").unwrap();
    let free = 70_000_000 - root;
    let needed = 30_000_000 - free;

    // Loop through the directories and find the smallest with enough space.
    let mut p2 = usize::MAX;
    for (_, &v) in sizes.iter() {
        if v > needed && v < p2 {
            p2 = v;
        }
    }

    println!("p2: {p2}");
}
