// in which I learn about the .cloned() for iters.

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};
fn main() {
    // Get the list.
    let f = File::open("input").unwrap();
    let buf = BufReader::new(f);
    let mut allergens = HashSet::new();
    let mut ingredients = HashSet::new();
    let list: Vec<(HashSet<String>, HashSet<String>)> = buf
        .lines()
        .map(|line| {
            let parts: Vec<String> = line
                .unwrap()
                .trim_end_matches(")")
                .split(" (contains ")
                .map(|l| l.to_string())
                .collect();
            let i = parts[0]
                .split(" ")
                .map(|l| {
                    let l = l.to_string();
                    ingredients.insert(l.clone());
                    l
                })
                .collect();
            let a = parts[1]
                .split(", ")
                .map(|l| {
                    let l = l.to_string();
                    allergens.insert(l.clone());
                    l
                })
                .collect();
            (i, a)
        })
        .collect();
    //println!("{:?}", list);
    // println!("{:?}", allergens);

    // Create a map of allergens to possible ingredients.
    let mut a2i: HashMap<String, HashSet<String>> = HashMap::new();
    for a in allergens {
        for (ingredients, allergens) in &list {
            if allergens.contains(&a) {
                if let Some(ii) = a2i.get_mut(&a) {
                    // We already have some possibilities. Get the
                    // intersection.
                    *ii = (*ii)
                        .intersection(&ingredients)
                        .cloned()
                        .collect::<HashSet<String>>();
                } else {
                    // First time seeing this one. Just add them all.
                    a2i.insert(
                        a.clone(),
                        ingredients.iter().cloned().collect::<HashSet<String>>(),
                    );
                }
            }
        }
    }
    // println!("a2i: {:?}", a2i);

    // Get a set of ingredients that have allergen.
    let iwa: HashSet<String> = a2i.values().flatten().cloned().collect();

    // Get a set that can't be allergies.
    let na: HashSet<String> = ingredients.difference(&iwa).cloned().collect();

    // we should now be able to iterator over the list and only count
    // ingredients that aren't allergies.
    let total: usize = na
        .iter()
        .map(|ingredient| {
            list.iter()
                .filter(|(ingredients, _)| ingredients.contains(&ingredient.clone()))
                .count()
        })
        .sum();
    println!("total: {}", total);

    // create the pairs. We search for one with a single ingredient,
    // add it to the list and then remove the others.
    let mut pairs: Vec<(String, String)> = vec![];
    // I tried to do this with partition and retain but
    // struggled. Here is a "normal" loop.
    while a2i.len() > 0 {
        let (a, r) = a2i
            .iter()
            .filter(|(_, ii)| ii.len() == 1)
            .map(|(a, r)| (a.clone(), r.iter().next().unwrap().clone()))
            .next()
            .unwrap();
        pairs.push((a.clone(), r.clone()));
        a2i.remove(&a.clone());
        for (_, ii) in a2i.iter_mut() {
            ii.remove(&r);
        }
    }
    // println!("pairs: {:?}", pairs);

    // Sort and combine for dangerous list.
    pairs.sort_by(|a, b| a.0.cmp(&b.0));
    let dangerous = pairs
        .iter()
        .map(|(_, b)| b.clone())
        .collect::<Vec<String>>()
        .join(",");
    println!("dangerous: {:?}", dangerous);
}
