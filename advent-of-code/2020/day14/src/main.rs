use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use std::collections::HashMap;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();
    let applicable =
        0b0000_0000_0000_0000_0000_0000_0000_1111_1111_1111_1111_1111_1111_1111_1111_1111;
    let mut zeros = applicable;
    let mut ones = 0;

    let mut values: HashMap<i64, i64> = HashMap::new();
    for line in aa.iter() {
        if line.starts_with("mask") {
            zeros = applicable;
            ones = 0;
            let mask = line.trim_start_matches("mask = ");
            for (i, c) in mask.chars().rev().enumerate() {
                if c == '0' {
                    zeros = zeros & !(1 << i);
                } else if c == '1' {
                    ones = ones | (1 << i);
                }
            }
            continue;
        }
        let parts = line.split("=").collect::<Vec<&str>>();
        let id = parts[0]
            .trim()
            .trim_start_matches("mem[")
            .trim_end_matches("]")
            .parse::<i64>()
            .unwrap();
        let value = parts[1].trim().parse::<i64>().unwrap();
        let value = value & zeros;
        let value = value | ones;
        values.insert(id, value & applicable);
    }

    let sum: i64 = values.iter().map(|(_, v)| v).sum();
    println!("{}", sum);

    ones = 0;
    values = HashMap::new();
    let mut floating: Vec<i64> = vec![];
    for line in aa.iter() {
        if line.starts_with("mask") {
            ones = 0;
            floating = vec![];
            let mask = line.trim_start_matches("mask = ");
            for (i, c) in mask.chars().rev().enumerate() {
                if c == '1' {
                    ones = ones | (1 << i);
                } else if c == 'X' {
                    floating.push(1 << i);
                }
            }
            continue;
        }

        let parts = line.split("=").collect::<Vec<&str>>();
        let id = parts[0]
            .trim()
            .trim_start_matches("mem[")
            .trim_end_matches("]")
            .parse::<i64>()
            .unwrap();
        let value = parts[1].trim().parse::<i64>().unwrap();
        float_all(&mut values, id | ones, value, &floating);
    }

    let sum: i64 = values.iter().map(|(_, v)| v).sum();
    println!("{}", sum);
}

fn float_all(values: &mut HashMap<i64, i64>, id: i64, value: i64, floating: &[i64]) {
    if floating.len() == 0 {
        values.insert(id, value);
        return;
    }
    float_all(values, id | floating[0], value, &floating[1..]);
    float_all(values, id & !floating[0], value, &floating[1..]);
}
