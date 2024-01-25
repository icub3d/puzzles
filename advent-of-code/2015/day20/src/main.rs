fn presents(max: usize, multiple: usize, limit: usize) -> usize {
    // look for the first house with more than max presents. Each elf
    // will deliver multiple presents to each house. Each elf will
    // stop delivery after deliverying to limit houses.
    let mut houses = vec![0; max / multiple + 1];
    for elf in 1..max / multiple + 1 {
        for house in (elf..max / multiple + 1).step_by(elf).take(limit) {
            houses[house] += elf * multiple;
        }
    }
    houses.iter().position(|&x| x >= max).unwrap()
}

fn main() {
    let input = 29000000;

    let now = std::time::Instant::now();
    println!(
        "p1: {:?} ({:?})",
        presents(input, 10, std::usize::MAX),
        now.elapsed()
    );
    let now = std::time::Instant::now();
    println!("p2: {:?} ({:?})", presents(input, 11, 50), now.elapsed());
}
