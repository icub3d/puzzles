use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let file = File::open("input").unwrap();
    let buf = BufReader::new(file);
    let aa: Vec<Vec<char>> = buf
        .lines()
        .map(|l| {
            let mut v: Vec<char> = vec![];
            for c in l.unwrap().chars() {
                v.push(c);
            }
            v
        })
        .collect();

    part1(&aa);
    part2(&aa);
}

fn part1(aa: &Vec<Vec<char>>) {
    let mut aa = aa.clone();
    loop {
        let cp = apply(&aa);
        if aa == cp {
            break;
        }
        aa = cp;
    }
    let mut occupied = 0;
    for a in aa.iter() {
        for c in a.iter() {
            if *c == '#' {
                occupied += 1;
            }
        }
    }

    let map: String = aa
        .iter()
        .map(|a| format!("{}\n", a.iter().collect::<String>()))
        .collect();
    println!("{}", map);
    println!("{:?}", occupied);
}

fn apply(aa: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut cp = aa.clone();
    let max_i = aa.len();
    let max_j = aa[0].len();
    for (i, a) in aa.iter().enumerate() {
        for (j, b) in a.iter().enumerate() {
            let adj = adjacent(aa, i as isize, j as isize, max_i as isize, max_j as isize);
            if *b == 'L' && adj == 0 {
                cp[i][j] = '#';
            } else if *b == '#' && adj >= 4 {
                cp[i][j] = 'L';
            }
        }
    }
    (*cp).to_vec()
}

fn adjacent(aa: &Vec<Vec<char>>, i: isize, j: isize, max_i: isize, max_j: isize) -> usize {
    let mut adj = 0;
    for x in -1..=1 as isize {
        if i + x < 0 || i + x >= max_i {
            continue;
        }
        for y in -1..=1 {
            if j + y < 0 || j + y >= max_j {
                continue;
            } else if x == 0 && y == 0 {
                continue;
            }

            if aa[(x + i) as usize][(y + j) as usize] == '#' {
                adj += 1;
            }
        }
    }
    adj
}

fn part2(aa: &Vec<Vec<char>>) {
    let mut aa = aa.clone();
    loop {
        let cp = apply2(&aa);
        if aa == cp {
            break;
        }
        aa = cp;
    }
    let mut occupied = 0;
    for a in aa.iter() {
        for c in a.iter() {
            if *c == '#' {
                occupied += 1;
            }
        }
    }

    let map: String = aa
        .iter()
        .map(|a| format!("{}\n", a.iter().collect::<String>()))
        .collect();
    println!("{}", map);
    println!("{:?}", occupied);
}

fn apply2(aa: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut cp = aa.clone();
    let max_i = aa.len();
    let max_j = aa[0].len();
    for (i, a) in aa.iter().enumerate() {
        for (j, b) in a.iter().enumerate() {
            let adj = visible(aa, i as isize, j as isize, max_i as isize, max_j as isize);
            if *b == 'L' && adj == 0 {
                cp[i][j] = '#';
            } else if *b == '#' && adj >= 5 {
                cp[i][j] = 'L';
            }
        }
    }
    (*cp).to_vec()
}

fn visible(aa: &Vec<Vec<char>>, i: isize, j: isize, max_i: isize, max_j: isize) -> usize {
    let mut adj = 0;
    for x in -1..=1 as isize {
        for y in -1..=1 as isize {
            if x == 0 && y == 0 {
                continue;
            }
            let v = visible_dir(aa, i, j, max_i, max_j, x, y);
            if v == '#' {
                adj += 1;
            }
        }
    }
    adj
}

fn visible_dir(
    aa: &Vec<Vec<char>>,
    i: isize,
    j: isize,
    max_i: isize,
    max_j: isize,
    delta_i: isize,
    delta_j: isize,
) -> char {
    let mut mul: isize = 1;
    loop {
        let ip = i + (delta_i * mul);
        let jp = j + (delta_j * mul);

        if ip >= max_i || jp >= max_j || ip < 0 || jp < 0 {
            return '.';
        }
        let c = aa[ip as usize][jp as usize];
        if c == '#' || c == 'L' {
            return c;
        }
        mul += 1;
    }
}
