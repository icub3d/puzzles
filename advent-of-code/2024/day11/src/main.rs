use cached::proc_macro::cached;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = include_str!("input.txt");
    let numbers = input
        .split_ascii_whitespace()
        .map(|w| w.parse::<usize>())
        .collect::<Result<Vec<usize>, std::num::ParseIntError>>()?;

    let now = std::time::Instant::now();
    let p1 = numbers.iter().map(|n| blink(*n, 25)).sum::<usize>();
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();
    let p2 = numbers.iter().map(|n| blink(*n, 75)).sum::<usize>();
    println!("p2: {} ({:?})", p2, now.elapsed());

    Ok(())
}

#[cached]
fn blink(number: usize, remaining: usize) -> usize {
    // If no blinks remain, we just return 1 because it represents one stone at the end.
    if remaining == 0 {
        return 1;
    }

    // Run the rules on the given stone.
    let s = number.to_string();
    if number == 0 {
        blink(1, remaining - 1)
    } else if s.len() % 2 == 0 {
        // Split the digits of the number in half and run blink on both halves.
        let half = s.len() / 2;
        let left = s[..half].parse::<usize>().unwrap();
        let right = s[half..].parse::<usize>().unwrap();
        blink(left, remaining - 1) + blink(right, remaining - 1)
    } else {
        // Otherwise we just multiply by 2024.
        blink(number * 2024, remaining - 1)
    }
}
