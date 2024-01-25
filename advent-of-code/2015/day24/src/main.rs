use icub3d_combinatorics::Combination;

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let mut weights: Vec<usize> = input.lines().map(|l| l.parse().unwrap()).collect();
    weights.sort_unstable_by(|a, b| b.cmp(a));

    let total_weight: usize = weights.iter().sum();
    let group_weight = total_weight / 3;
    println!("info: {} {} {}", total_weight, group_weight, weights.len());
    println!("{:?}", weights);

    let now = std::time::Instant::now();
    let candidates = Combination::new(weights.len(), 6)
        .map(|c| c.iter().map(|&i| weights[i]).collect::<Vec<_>>())
        .filter(|c| c.iter().sum::<usize>() == group_weight)
        .map(|c| c.iter().product::<usize>())
        .min();
    println!("p1: {:?} ({:?})", candidates, now.elapsed());

    let total_weight: usize = weights.iter().sum();
    let group_weight = total_weight / 4;
    println!("new-weight: {}", group_weight);

    let now = std::time::Instant::now();
    let candidates = Combination::new(weights.len(), 4)
        .map(|c| c.iter().map(|&i| weights[i]).collect::<Vec<_>>())
        .filter(|c| c.iter().sum::<usize>() == group_weight)
        .map(|c| c.iter().product::<usize>())
        .min();
    println!("p2: {:?} ({:?})", candidates, now.elapsed());
}
