use std::collections::{HashMap, HashSet};

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let mut distances: HashMap<&str, HashMap<&str, usize>> = HashMap::new();

    // Input distances for each.
    input.lines().for_each(|line| {
        let parts = line.split(' ').collect::<Vec<&str>>();
        distances
            .entry(&parts[0])
            .and_modify(|m| {
                m.insert(parts[2], parts[4].parse().unwrap());
            })
            .or_insert_with(|| {
                let mut m = HashMap::new();
                m.insert(parts[2], parts[4].parse().unwrap());
                m
            });
        distances
            .entry(&parts[2])
            .and_modify(|m| {
                m.insert(parts[0], parts[4].parse().unwrap());
            })
            .or_insert_with(|| {
                let mut m = HashMap::new();
                m.insert(parts[0], parts[4].parse().unwrap());
                m
            });
    });

    let mut s = usize::MAX;
    let mut l = usize::MIN;
    for cur in distances.keys() {
        s = s.min(minmax(
            &distances,
            &mut HashSet::from([*cur]),
            cur,
            |a, b| match a < b {
                true => a,
                false => b,
            },
            usize::MAX,
        ));
        l = l.max(minmax(
            &distances,
            &mut HashSet::from([*cur]),
            cur,
            |a, b| match a > b {
                true => a,
                false => b,
            },
            usize::MIN,
        ));
    }
    println!("p1: {s}");
    println!("p2: {l}");
}

fn minmax<'a>(
    distances: &HashMap<&'a str, HashMap<&'a str, usize>>,
    seen: &mut HashSet<&'a str>,
    cur: &'a str,
    f: fn(usize, usize) -> usize,
    start: usize,
) -> usize {
    if seen.len() == distances.len() {
        return 0;
    }

    let mut s = start;
    for next in distances.get(cur).unwrap().keys() {
        if seen.contains(next) {
            continue;
        }
        seen.insert(next);
        s = f(
            s,
            distances[cur][next] + minmax(&distances, seen, next, f, start),
        );
        seen.remove(next);
    }

    s
}
