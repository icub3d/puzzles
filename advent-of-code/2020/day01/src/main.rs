use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<i64> = buf
        .lines()
        .map(|l| l.unwrap().parse::<i64>().unwrap())
        .collect();

    println!("sum 2 2020: {}", sum_2_2020(&aa));
    println!("sum 3 2020: {}", sum_3_2020(&aa));
}

fn sum_2_2020(aa: &[i64]) -> i64 {
    for (i, a) in aa.iter().enumerate() {
        for b in aa[i..].iter() {
            if a + b == 2020 {
                return a * b;
            }
        }
    }
    0
}

fn sum_3_2020(aa: &[i64]) -> i64 {
    for (i, a) in aa.iter().enumerate() {
        for (j, b) in aa[i..].iter().enumerate() {
            for c in aa[j..].iter() {
                if a + b + c == 2020 {
                    return a * b * c;
                }
            }
        }
    }
    0
}
