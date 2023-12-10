use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<u64> = buf
        .lines()
        .map(|l| (l.unwrap().parse::<u64>().unwrap()))
        .collect();

    let mut min = 0;
    let mut max = 25;
    let mut invalid = 0;
    for i in max..aa.len() {
        match valid(&aa[min..max], aa[i]) {
            Ok((_, _)) => {}
            Err(_) => {
                invalid = aa[max];
                println!("invalid: {}", aa[max]);
                break;
            }
        }
        min += 1;
        max += 1;
    }

    let c = contiguous(aa, invalid);
    println!("contiguous: {} {} {}", c.0, c.1, c.0 + c.1);
}

fn contiguous(aa: Vec<u64>, want: u64) -> (u64, u64) {
    for x in 0..aa.len() {
        let mut cur = x;
        let mut total: u64 = 0;
        while total < want {
            total += aa[cur];
            if total == want {
                return (smallest(&aa[x..=cur]), largest(&aa[x..=cur]));
            }
            cur += 1;
        }
    }
    return (0, 0);
}

fn smallest(aa: &[u64]) -> u64 {
    let mut s = aa[0];
    for x in aa[1..].iter() {
        if x < &s {
            s = *x;
        }
    }
    s
}

fn largest(aa: &[u64]) -> u64 {
    let mut s = aa[0];
    for x in aa[1..].iter() {
        if x > &s {
            s = *x;
        }
    }
    s
}

fn valid(aa: &[u64], want: u64) -> Result<(usize, usize), bool> {
    for (i, v) in aa.iter().enumerate() {
        for (j, w) in aa[i + 1..].iter().enumerate() {
            if v + w == want {
                return Ok((i, j));
            }
        }
    }
    Err(false)
}
