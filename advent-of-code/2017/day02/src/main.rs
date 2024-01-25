fn main() {
    let input = include_str!("../input");
    let p1 = input
        .lines()
        .map(|line| {
            let mut nums = line
                .split_whitespace()
                .map(|num| num.parse::<u32>().unwrap())
                .collect::<Vec<u32>>();
            nums.sort();
            nums.last().unwrap() - nums.first().unwrap()
        })
        .sum::<u32>();
    println!("p1: {}", p1);

    let p2 = input
        .lines()
        .map(|line| {
            let mut nums = line
                .split_whitespace()
                .map(|num| num.parse::<u32>().unwrap())
                .collect::<Vec<u32>>();
            nums.sort();
            for i in 0..nums.len() {
                for j in i + 1..nums.len() {
                    if nums[j] % nums[i] == 0 {
                        return nums[j] / nums[i];
                    }
                }
            }
            0
        })
        .sum::<u32>();
    println!("p2: {}", p2);
}
