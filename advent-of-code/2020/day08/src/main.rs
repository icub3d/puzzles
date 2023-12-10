use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<(bool, String)> = buf.lines().map(|l| (false, l.unwrap())).collect();

    let mut tmp = aa.clone();
    match valid(&mut tmp) {
        Ok(v) => println!("ended at: {}", v),
        Err(v) => println!("repeat at: {}", v),
    }

    for (i, op) in aa.iter().enumerate() {
        if op.1.starts_with("nop") {
            let mut tmp = aa.clone();
            tmp[i].1 = op.1.replace("nop", "jmp");
            match valid(&mut tmp) {
                Ok(v) => {
                    println!("found at {}: {}", i, v);
                    break;
                }
                Err(_) => continue,
            }
        } else if op.1.starts_with("jmp") {
            let mut tmp = aa.clone();
            tmp[i].1 = op.1.replace("jmp", "nop");
            match valid(&mut tmp) {
                Ok(v) => {
                    println!("found at {}: {}", i, v);
                    break;
                }
                Err(_) => continue,
            }
        }
    }
}

fn valid(aa: &mut Vec<(bool, String)>) -> Result<i64, i64> {
    let mut cur: isize = 0;
    let mut max: isize = 0;
    let mut acc: i64 = 0;

    loop {
        if cur as usize >= aa.len() {
            return Ok(acc);
        }
        if cur > max {
            max = cur;
        }
        let mut op = &mut aa[cur as usize];
        if op.0 {
            return Err(acc);
        }
        op.0 = true;

        let parts: Vec<&str> = op.1.split(" ").collect();
        match parts[0] {
            "nop" => {
                cur += 1;
            }
            "acc" => {
                cur += 1;
                acc += parts[1].parse::<i64>().unwrap();
            }
            "jmp" => {
                cur += parts[1].parse::<isize>().unwrap();
            }
            _ => {
                panic!("unknown instruction: {}", parts[0]);
            }
        }
    }
}
