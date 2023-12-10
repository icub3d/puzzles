use std::fs;

fn main() {
    let lines = fs::read_to_string("input").unwrap();
    let lines = lines.lines().collect::<Vec<&str>>();

    let mut x: isize = 1;
    let mut cycle = 0;
    let mut check = 20;
    let mut p1 = 0;
    let mut instruction = 0;
    let mut in_addx = false;
    let mut p2 = vec![vec!["."; 40]; 6];

    loop {
        if instruction >= lines.len() {
            break;
        }
        cycle += 1;

        // Do our "during" stuff here?
        if cycle == check {
            check += 40;
            p1 += cycle * x;
        }
        let row = (cycle - 1) / 40;
        let col = (cycle - 1) % 40;
        if x == (col + 1) || x == col || x == (col - 1) {
            p2[row as usize][col as usize] = "#";
        }

        let parts = lines[instruction].split(' ').collect::<Vec<&str>>();
        if in_addx {
            in_addx = false;
            x += parts[1].parse::<isize>().unwrap();
            instruction += 1;
        } else if parts[0] == "noop" {
            instruction += 1;
        } else {
            in_addx = true;
        }
    }
    println!("p1: {p1}");
    println!("p2:");
    p2.iter().for_each(|v| println!("{}", v.join("")));
}
