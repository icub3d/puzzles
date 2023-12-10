use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<(char, i64)> = buf
        .lines()
        .map(|l| {
            let s = l.unwrap();
            let dir = s.chars().next().unwrap();
            let value = s[1..].parse::<i64>().unwrap();
            (dir, value)
        })
        .collect();

    let mut facing: char = 'E';
    let mut ns: i64 = 0;
    let mut ew: i64 = 0;

    for (dir, value) in aa.iter() {
        match dir {
            'L' => facing = rotate(*dir, *value, facing),
            'R' => facing = rotate(*dir, *value, facing),
            'F' => mv(facing, &mut ns, &mut ew, *value, 1, 1),
            _ => mv(*dir, &mut ns, &mut ew, *value, 1, 1),
        }
    }
    println!("{} {} {}", ns, ew, ns.abs() + ew.abs());
    println!("");

    ns = 0;
    ew = 0;
    let mut ns_wp: i64 = 1;
    let mut ew_wp: i64 = 10;
    for (dir, value) in aa.iter() {
        match dir {
            'L' => rotate_wp(*dir, *value, &mut ns_wp, &mut ew_wp),
            'R' => rotate_wp(*dir, *value, &mut ns_wp, &mut ew_wp),
            'F' => {
                ns += ns_wp * value;
                ew += ew_wp * value;
            }
            _ => mv(*dir, &mut ns_wp, &mut ew_wp, *value, 1, 1),
        }
    }
    println!("{} {} {}", ns, ew, ns.abs() + ew.abs());
}

fn rotate_wp(dir: char, value: i64, ns_wp: &mut i64, ew_wp: &mut i64) {
    let mut value = value;
    while value > 0 {
        // Swap values
        let tmp = *ns_wp;
        *ns_wp = *ew_wp;
        *ew_wp = tmp;

        // change sign
        if dir == 'R' {
            *ns_wp = *ns_wp * -1;
        } else {
            *ew_wp = *ew_wp * -1;
        }
        value -= 90;
    }
}

fn rotate(dir: char, value: i64, cur: char) -> char {
    let mut cur = cur;
    let mut value = value;
    while value > 0 {
        cur = match dir {
            'L' => match cur {
                'E' => 'N',
                'N' => 'W',
                'W' => 'S',
                'S' => 'E',
                _ => 'E',
            },
            'R' => match cur {
                'E' => 'S',
                'N' => 'E',
                'W' => 'N',
                'S' => 'W',
                _ => 'E',
            },
            _ => 'E',
        };
        value -= 90;
    }
    cur
}

fn mv(dir: char, ns: &mut i64, ew: &mut i64, value: i64, ns_mul: i64, ew_mul: i64) {
    match dir {
        'N' => *ns += value * ns_mul,
        'S' => *ns -= value * ns_mul,
        'E' => *ew += value * ew_mul,
        'W' => *ew -= value * ew_mul,
        _ => (),
    }
}
