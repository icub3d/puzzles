fn main() {
    let input = include_str!("../input");
    let p1 = input
        .trim()
        .chars()
        .zip(input.trim().chars().cycle().skip(1))
        .fold(0, |acc, (a, b)| {
            if a == b {
                acc + a.to_digit(10).unwrap()
            } else {
                acc
            }
        });
    println!("p1: {}", p1);

    let p2 = input
        .trim()
        .chars()
        .zip(input.trim().chars().cycle().skip(input.trim().len() / 2))
        .fold(0, |acc, (a, b)| {
            if a == b {
                acc + a.to_digit(10).unwrap()
            } else {
                acc
            }
        });
    println!("p1: {}", p2);
}
