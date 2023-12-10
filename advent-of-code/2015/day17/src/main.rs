fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let mut containers = input
        .lines()
        .map(|l| l.parse::<u32>().unwrap())
        .collect::<Vec<_>>();
    containers.sort_unstable_by(|a, b| b.cmp(a));

    let mut min = u32::MAX;
    println!("p1: {}", count(&containers, 150, &mut min, 0));
    println!("p2: {}", p2(&containers, 150, min, 0));
}

fn count(containers: &[u32], target: u32, min: &mut u32, used: u32) -> u32 {
    if target == 0 {
        if used < *min {
            *min = used;
        }
        return 1;
    }

    let mut total = 0;
    for (i, &c) in containers.iter().enumerate() {
        if c <= target {
            total += count(&containers[i + 1..], target - c, min, used + 1);
        }
    }
    total
}

fn p2(containers: &[u32], target: u32, max: u32, used: u32) -> u32 {
    if target == 0 {
        if used == max {
            return 1;
        }
        return 0;
    }

    let mut total = 0;
    for (i, &c) in containers.iter().enumerate() {
        if c <= target {
            total += p2(&containers[i + 1..], target - c, max, used + 1);
        }
    }
    total
}
