fn calculate_fuel(mass: isize) -> isize {
    // Calculate the fuel for the mass.
    let fuel = mass / 3 - 2;

    // If we have zero or negative fuel, we are done.
    if fuel <= 0 {
        return 0;
    }

    // Otherwise, we need to calculate the fuel for the fuel.
    fuel + calculate_fuel(fuel)
}
fn main() {
    let input = include_str!("../input");

    // For part 1, we can just use an iterator to calculate the fuel for each inline and add them
    // togehter. We assume valid input, so we can just unwrap the parse.
    let p1 = input
        .lines()
        .map(|l| l.parse::<usize>().unwrap() / 3 - 2)
        .sum::<usize>();
    println!("p1: {}", p1);

    // For part 2, we can use recursion to calculate the fuel for the fuel. Otherwise iterating
    // looks the same. We use isize though to simplify checking for zero or negative fuel.
    let p2 = input
        .lines()
        .map(|l| calculate_fuel(l.parse::<isize>().unwrap()))
        .sum::<isize>();
    println!("p2: {}", p2);
}
