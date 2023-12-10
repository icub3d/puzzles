use std::collections::{HashMap, HashSet};

use icub3d_combinatorics::Permutation;

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let input = input
        .lines()
        .map(|line| {
            let parts = line.split(" ").collect::<Vec<_>>();
            let left = parts[0].chars().nth(0).unwrap();
            let right = parts[10].chars().nth(0).unwrap();
            let value = parts[3].parse::<isize>().unwrap();
            let value = if parts[2] == "lose" { -value } else { value };
            ((left, right), value)
        })
        .collect::<HashMap<(char, char), isize>>();
    println!("{:?}", &input);

    let people = input
        .keys()
        .map(|(a, _)| *a)
        .collect::<HashSet<_>>()
        .iter()
        .cloned()
        .collect::<Vec<_>>();
    dbg!(&people);

    // Start with permutations since they appear to only be 8.
    let mut max = 0;
    for perm in Permutation::new(people.len()) {
        max = max.max(happiness(&perm, &input, &people));
    }
    println!("p1: {}", max);

    // Add myself to the list of people.
    let people = {
        let mut p = people;
        p.push('X');
        p
    };
    let input = {
        let mut i = input;
        for person in &people {
            i.insert(('X', *person), 0);
            i.insert((*person, 'X'), 0);
        }
        i
    };

    // Start with permutations since they appear to only be 8.
    let mut max = 0;
    for perm in Permutation::new(people.len()) {
        max = max.max(happiness(&perm, &input, &people));
    }
    println!("p2: {}", max);
}

fn happiness(perm: &Vec<usize>, input: &HashMap<(char, char), isize>, people: &[char]) -> isize {
    let mut total = 0;
    for i in 0..perm.len() {
        let cur = people[perm[(i + 1) % perm.len()]];
        let left = people[perm[i]];
        let right = people[perm[(i + 2) % perm.len()]];

        total += input.get(&(cur, right)).unwrap();
        total += input.get(&(cur, left)).unwrap();
    }
    total
}
