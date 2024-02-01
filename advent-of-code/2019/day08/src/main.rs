fn main() {
    let input = include_str!("../input");
    let digits = input
        .trim_end()
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect::<Vec<_>>();

    let p1 = digits
        .chunks(25 * 6)
        .map(|layer| {
            let mut count = [0; 3];
            for &d in layer {
                count[d as usize] += 1;
            }
            count
        })
        .min_by_key(|count| count[0])
        .unwrap();
    println!("Part 1: {}", p1[1] * p1[2]);

    let p2 = digits
        .chunks(25 * 6)
        .fold(vec![2; 25 * 6], |mut acc, layer| {
            for (i, &d) in layer.iter().enumerate() {
                if acc[i] == 2 {
                    acc[i] = d;
                }
            }
            acc
        });

    println!("Part 2:");
    for row in p2.chunks(25) {
        for &d in row {
            print!("{}", if d == 1 { "█" } else { " " });
        }
        println!();
    }
}
