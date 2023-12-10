use std::fs;

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let pairs = lines
        .lines()
        .map(|s| s.split(',').collect::<Vec<&str>>())
        .map(|v| {
            let left = v[0]
                .split('-')
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<usize>>();
            let right = v[1]
                .split('-')
                .map(|n| n.parse::<usize>().unwrap())
                .collect::<Vec<usize>>();
            ((left[0], left[1]), (right[0], right[1]))
        })
        .collect::<Vec<((usize, usize), (usize, usize))>>();

    let mut p1 = 0;
    for pair in pairs.clone() {
        if pair.0 .0 >= pair.1 .0 && pair.0 .1 <= pair.1 .1 {
            p1 += 1;
        } else if pair.1 .0 >= pair.0 .0 && pair.1 .1 <= pair.0 .1 {
            p1 += 1;
        }
    }
    println!("p1: {p1}");

    let mut p2 = 0;
    for pair in pairs {
        if pair.0 .0 >= pair.1 .0 && pair.0 .0 <= pair.1 .1 {
            p2 += 1;
        } else if pair.0 .1 >= pair.1 .0 && pair.0 .1 <= pair.1 .1 {
            p2 += 1;
        } else if pair.1 .0 >= pair.0 .0 && pair.1 .0 <= pair.0 .1 {
            p2 += 1;
        } else if pair.1 .1 >= pair.0 .0 && pair.1 .1 <= pair.0 .1 {
            p2 += 1;
        }
    }
    println!("p2: {p2}");
}
