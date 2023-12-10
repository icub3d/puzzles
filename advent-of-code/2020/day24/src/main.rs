use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};

fn moves(s: String) -> Vec<String> {
    let mut mm = vec![];

    let mut it = s.chars();
    loop {
        let cur = it.next();
        match cur {
            None => break,
            Some(c) => {
                if c == 'n' || c == 's' {
                    mm.push(vec![c, it.next().unwrap()].iter().collect::<String>());
                } else {
                    mm.push(c.to_string());
                }
            }
        }
    }

    mm
}

fn main() {
    let br = BufReader::new(File::open("input").unwrap());

    // We'll parse the line with these and then track it's position
    // with the deltas.
    let dirs = vec!["ne", "nw", "se", "sw", "e", "w"];
    let deltas = vec![(0, 1), (-1, 1), (1, -1), (0, -1), (1, 0), (-1, 0)];
    let mut directions = HashMap::new();
    for (dir, delta) in dirs.iter().zip(deltas.iter()) {
        directions.insert(dir.to_string(), delta);
    }

    // We'll track the tiles that are turned on in a set.
    let mut black = HashSet::new();

    // Iterate through all the lines.
    for l in br.lines() {
        let mut cur = (0, 0);
        let mvs = moves(l.unwrap());
        for mv in mvs {
            let delta = directions[&mv];
            cur.0 += delta.0;
            cur.1 += delta.1;
        }
        if black.contains(&cur) {
            black.remove(&cur);
        } else {
            black.insert(cur.clone());
        }
    }
    println!("{:?}", black.len());

    for _ in 0..100 {
        // We now need to track white neighbors and black tiles we
        // don't want to flip.
        let mut white = HashMap::new();
        let mut keep = HashSet::new();

        // Loop through our current tileset.
        for tile in black.iter() {
            // Find neighbors or update white neighbors.
            let mut neighbors = 0;
            for (_, delta) in directions.iter() {
                let neighbor = (tile.0 + delta.0, tile.1 + delta.1);
                if black.contains(&neighbor) {
                    neighbors += 1;
                } else {
                    *white.entry(neighbor).or_insert(0) += 1;
                }
            }

            // Should this one stay black?
            if neighbors == 1 || neighbors == 2 {
                keep.insert(tile.clone());
            }
        }
        let w2b = white
            .iter()
            .filter(|(_, count)| **count == 2)
            .map(|(tile, _)| tile)
            .cloned()
            .collect::<HashSet<(isize, isize)>>();
        black = keep
            .union(&w2b)
            .cloned()
            .collect::<HashSet<(isize, isize)>>();
    }
    println!("{}", black.len());
}
