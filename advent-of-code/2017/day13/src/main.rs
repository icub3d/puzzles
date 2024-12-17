fn main() {
    let input = include_str!("../input");
    let layers = input
        .lines()
        .map(|line| {
            let mut parts = line.split(": ");
            let depth = parts.next().unwrap().parse().unwrap();
            let range = parts.next().unwrap().parse().unwrap();
            (depth, range)
        })
        .collect::<Vec<(usize, usize)>>();

    let severity = layers
        .iter()
        .filter(|&&(depth, range)| depth % (2 * (range - 1)) == 0)
        .map(|&(depth, range)| depth * range)
        .sum::<usize>();
    println!("p1: {}", severity);

    let delay = (0..)
        .find(|&delay| !hit(&layers, delay))
        .unwrap();
    println!("p2: {}", delay);
}

fn hit(layers: &[(usize, usize)], delay: usize) -> bool {
    layers.iter().any(|&(depth, range)| (depth + delay) % (2 * (range - 1)) == 0)
}