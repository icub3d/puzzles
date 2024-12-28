use std::{collections::HashMap, iter::once};

use rayon::prelude::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");

    // Find the 2000th secret for each value and sum them up.
    let now = std::time::Instant::now();
    let p1 = input
        .lines()
        .filter_map(|line| line.parse::<isize>().ok())
        .map(|secret| nth_secret(secret, 2000))
        .sum::<isize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    // For each initial value, find all the sequences and then collect all of
    // them into a counter.  The maximum value in the counter is the answer.
    let now = std::time::Instant::now();
    let counts = input
        .lines()
        .par_bridge()
        .filter_map(|line| line.parse::<isize>().ok())
        .flat_map(|n| n_sequences(n, 2000))
        .fold(
            || HashMap::new(),
            |mut counts, (k, v)| {
                // This is going to happen on chunks of the flat_map above.
                *counts.entry(k).or_insert(0) += v;
                counts
            },
        )
        .reduce(HashMap::new, |mut a, b| {
            // Reduce the counts into a single HashMap.
            for (k, v) in b {
                *a.entry(k).or_insert(0) += v;
            }
            a
        });
    let p2 = counts.values().max().ok_or("no max value found")?;
    println!("p2: {} ({:?})", p2, now.elapsed());

    Ok(())
}

// Get the next secret. We basically just follow the instruction here.
fn next(mut secret: isize) -> isize {
    secret = ((secret * 64) ^ secret) % 16_777_216;
    secret = ((secret / 32) ^ secret) % 16_777_216;
    ((secret * 2048) ^ secret) % 16_777_216
}

// Get the nth secret (for part 1).
fn nth_secret(secret: isize, n: isize) -> isize {
    // We can leverage fold here by just passing the secret over and over again
    // to next().
    (0..n).fold(secret, |n, _| next(n))
}

// Calculate the prices and their differences over n secret values. Return only
// to first value of a sequence.
fn n_sequences(secret: isize, n: isize) -> HashMap<Vec<isize>, isize> {
    // First collect the prices. We'll use these to create our windows and find
    // the price of each sequence.
    let prices = once(secret % 10)
        .chain((0..n).scan(secret, |state, _| {
            let next = next(*state);
            *state = next;
            Some(next % 10)
        }))
        .collect::<Vec<isize>>();

    // Calculate the differences between the prices.
    let differences = prices
        .windows(2)
        .map(|w| w[1] - w[0])
        .collect::<Vec<isize>>();

    // Create our sequences and store the first price of each sequence.
    differences
        .windows(4)
        .enumerate()
        .fold(HashMap::new(), |mut sequences, (i, w)| {
            // We only want to keep the first time we see a sequence per the
            // problem.
            let key = w.to_vec();
            if !sequences.contains_key(&key) {
                sequences.insert(key, prices[i + 4]);
            }
            sequences
        })
}
