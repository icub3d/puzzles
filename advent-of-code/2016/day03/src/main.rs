fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let triangles = input
        .lines()
        .map(|line| {
            let mut sides = line.split_whitespace().map(|s| s.parse::<u32>().unwrap());
            (
                sides.next().unwrap(),
                sides.next().unwrap(),
                sides.next().unwrap(),
            )
        })
        .collect::<Vec<_>>();

    let valid = triangles
        .iter()
        .filter(|&(a, b, c)| *a + *b > *c && *a + *c > *b && *b + *c > *a)
        .count();
    println!("p1: {}", valid);

    let mut valid = 0;
    for group in triangles.chunks(3) {
        let (a, b, c) = (group[0].0, group[1].0, group[2].0);
        if a + b > c && a + c > b && b + c > a {
            valid += 1;
        }
        let (a, b, c) = (group[0].1, group[1].1, group[2].1);
        if a + b > c && a + c > b && b + c > a {
            valid += 1;
        }
        let (a, b, c) = (group[0].2, group[1].2, group[2].2);
        if a + b > c && a + c > b && b + c > a {
            valid += 1;
        }
    }
    println!("p2: {}", valid);
}
