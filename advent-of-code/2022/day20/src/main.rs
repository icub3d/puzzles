fn main() {
    let input = include_str!("../input");
    let numbers = input
        .lines()
        .map(|line| line.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();

    let ip = numbers.iter().enumerate().collect();
}

fn shuffle(numbers: Vec<(usize, i32)>, count: usize) -> Vec<(usize, i32)> {}
