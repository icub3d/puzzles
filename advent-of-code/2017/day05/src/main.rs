fn main() {
    let input = include_str!("../input");
    let jumps = input
        .lines()
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();

    let mut jumps1 = jumps.clone();
    let steps1 = run(&mut jumps1, |x| x + 1);
    println!("p1: {}", steps1);

    let mut jumps2 = jumps.clone();
    let steps2 = run(&mut jumps2, |x| if x >= 3 { x - 1 } else { x + 1 });
    println!("p2: {}", steps2);
}

type Incrementer = fn(i32) -> i32;

fn run(jumps: &mut [i32], incrementer: Incrementer) -> usize {
    let mut steps = 0;
    let mut pc = 0;
    while pc < jumps.len() {
        let offset = jumps[pc];
        jumps[pc] = incrementer(offset);
        pc = (pc as i32 + offset) as usize;
        steps += 1;
    }
    steps
}
