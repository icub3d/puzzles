use itertools::Itertools;

fn main() {
    // Parse input
    let input = include_str!("input.txt");
    let (mut left, mut right): (Vec<i64>, Vec<i64>) = input.lines().map(
        |x| {
            let mut pairs = x.split_whitespace();
            (pairs.next()?.parse::<i64>().unwrap(), pairs.next().unwrap().parse::<i64>().unwrap())
        }
    ).unzip();

    // Sort the input
    left.sort_unstable();
    right.sort_unstable();

    // Find the sum of the differences for part 1. 
    let p1 = left.iter().zip(right.iter()).map(|(l,r)| (l-r).abs()).sum::<i64>();
    println!("p1: {}", p1);

    // Find the product of the counts of the left values in the right list for part 2.
    let map = right.iter().counts();
    let p2 = left.iter().map(|x| x * *map.get(x).unwrap_or(&0) as i64).sum::<i64>();
    println!("p2: {}", p2);
}