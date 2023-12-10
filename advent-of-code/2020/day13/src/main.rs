use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<String> = buf.lines().map(|l| l.unwrap()).collect();

    let cur = aa[0].parse::<i64>().unwrap();
    let min: (i64, i64) = aa[1]
        .split(',')
        .filter(|&x| x != "x")
        .map(|l| {
            let l = l.parse::<i64>().unwrap();
            (l, ((cur / l) + 1) * l)
        })
        .min_by(|x, y| x.1.cmp(&y.1))
        .unwrap();
    println!("{:?} {}", min, min.0 * (min.1 - cur));

    let buses: Vec<(i64, i64)> = aa[1]
        .split(',')
        .enumerate()
        .filter(|(_, x)| *x != "x")
        .map(|(i, x)| (i as i64, x.parse::<i64>().unwrap()))
        .collect();
    let res: Vec<i64> = buses.iter().map(|&(i, x)| x - i).collect();
    let mods: Vec<i64> = buses.iter().map(|&(_, x)| x).collect();
    println!("{}", cr(&res, &mods))
}

// Chinese Remainder Thereom - https://en.wikipedia.org/wiki/Chinese_remainder_theorem
fn cr(res: &[i64], mods: &[i64]) -> i64 {
    let prod: i64 = mods.iter().product();
    let mut sum = 0;
    for (&r, &m) in res.iter().zip(mods) {
        let p = prod / m;
        sum += r * mod_inv(p, m) * p
    }
    sum % prod
}

// Normally there might not be one, but we assume the program input is
// good.
fn mod_inv(x: i64, n: i64) -> i64 {
    let (_, x, _) = egcd(x, n);
    (x % n + n) % n
}

fn egcd(a: i64, b: i64) -> (i64, i64, i64) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}
