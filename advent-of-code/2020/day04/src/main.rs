use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref HCL: Regex = Regex::new("^#[0-9a-fA-F]{6}$").unwrap();
    static ref PID: Regex = Regex::new("^[0-9]{9}$").unwrap();
    static ref REQUIRED: Vec<&'static str> = vec!["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
    static ref EYE_COLOR: Vec<&'static str> = vec!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
}

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let mut m = HashMap::new();
    let mut good1 = 0;
    let mut good2 = 0;
    let lines = buf.lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    for l in lines.iter() {
        let l = l.trim();
        if l == "" {
            if valid1(&m) {
                good1 += 1;
            }
            if valid2(&m) {
                good2 += 1;
            }
            m.drain();
            continue;
        }

        for part in l.split(" ").collect::<Vec<&str>>().iter() {
            let parts: Vec<&str> = part.split(":").collect();
            m.insert(parts[0].clone(), parts[1].clone());
        }
    }
    if valid1(&m) {
        good1 += 1;
    }
    if valid2(&m) {
        good2 += 1;
    }

    println!("good 1: {}", good1);
    println!("good 2: {}", good2);
}

pub fn valid1(m: &HashMap<&str, &str>) -> bool {
    for key in REQUIRED.iter() {
        if !m.contains_key(key) {
            return false;
        }
    }
    true
}

pub fn valid2(m: &HashMap<&str, &str>) -> bool {
    for key in REQUIRED.iter() {
        if !m.contains_key(key) {
            return false;
        }
    }

    let byr = m["byr"].parse::<i64>().unwrap();
    if byr < 1920 || byr > 2002 {
        return false;
    }

    let iyr = m["iyr"].parse::<i64>().unwrap();
    if iyr < 2010 || iyr > 2020 {
        return false;
    }

    let eyr = m["eyr"].parse::<i64>().unwrap();
    if eyr < 2020 || eyr > 2030 {
        return false;
    }

    let hgt = m["hgt"];
    if hgt.ends_with("cm") {
        let hgt = &hgt[..hgt.len() - 2];
        let hgt = hgt.parse::<i64>().unwrap();
        if hgt < 150 || hgt > 193 {
            return false;
        }
    } else if hgt.ends_with("in") {
        let hgt = &hgt[..hgt.len() - 2];
        let hgt = hgt.parse::<i64>().unwrap();
        if hgt < 59 || hgt > 76 {
            return false;
        }
    } else {
        return false;
    }

    let hcl = m["hcl"];
    if !HCL.is_match(hcl) {
        return false;
    }

    let pid = m["pid"];
    if !PID.is_match(pid) {
        return false;
    }

    let ecl = m["ecl"];
    let mut found = false;
    for clr in EYE_COLOR.iter() {
        if ecl == *clr {
            found = true;
            break;
        }
    }

    found
}
