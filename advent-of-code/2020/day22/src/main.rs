use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let br = BufReader::new(File::open("input").unwrap());
    let mut players = vec![VecDeque::<usize>::new(), VecDeque::<usize>::new()];
    let mut cur = 0;

    for l in br.lines() {
        let l = l.unwrap();
        if l == "Player 1:" || l == "" {
            continue;
        } else if l == "Player 2:" {
            cur = 1;
        } else {
            players[cur].push_back(l.parse::<usize>().unwrap());
        }
    }

    let mut p1 = players[0].clone();
    let mut p2 = players[1].clone();
    let winner = match game(&mut p1, &mut p2, 1) {
        true => p1.clone(),
        false => p2.clone(),
    };
    let score: usize = winner
        .iter()
        .rev()
        .enumerate()
        .map(|(i, v)| v * (i + 1))
        .sum();
    println!("score: {:?}", score);
}

fn game(p1: &mut VecDeque<usize>, p2: &mut VecDeque<usize>, depth: usize) -> bool {
    let mut p1s: HashSet<VecDeque<usize>> = HashSet::new();
    let mut p2s: HashSet<VecDeque<usize>> = HashSet::new();
    while !p1.is_empty() && !p2.is_empty() {
        if !p1s.insert(p1.clone()) && !p2s.insert(p2.clone()) {
            return true;
        }

        let c1 = p1.pop_front().unwrap();
        let c2 = p2.pop_front().unwrap();

        if p1.len() >= c1 && p2.len() >= c2 {
            // This triggers a sub-game.
            match game(
                &mut p1.iter().take(c1).cloned().collect(),
                &mut p2.iter().take(c2).cloned().collect(),
                depth + 1,
            ) {
                true => {
                    p1.push_back(c1);
                    p1.push_back(c2);
                }
                false => {
                    p2.push_back(c2);
                    p2.push_back(c1);
                }
            }
        } else if c1 > c2 {
            p1.push_back(c1);
            p1.push_back(c2);
        } else {
            p2.push_back(c2);
            p2.push_back(c1);
        }
    }

    !p1.is_empty()
}
