fn dragon_curve(a: &str) -> String {
    let b = a
        .chars()
        .rev()
        .map(|c| if c == '0' { '1' } else { '0' })
        .collect::<String>();
    format!("{}0{}", a, b)
}

fn dragon_curve_n(a: &str, n: usize) -> String {
    let mut a = a.to_string();
    while a.len() < n {
        a = dragon_curve(&a);
    }
    a.chars().take(n).collect()
}

fn checksum(data: &str) -> String {
    let mut checksum = data.to_string();
    loop {
        checksum = checksum
            .chars()
            .collect::<Vec<_>>()
            .chunks(2)
            .map(|c| if c[0] == c[1] { '1' } else { '0' })
            .collect();

        if checksum.len() % 2 != 0 {
            break;
        }
    }
    checksum
}

fn main() {
    let input = "10001110011110000";
    // let input = "10000";
    let n = 272;
    let data = dragon_curve_n(input, n);
    println!("p1: {}", checksum(&data));

    let n = 35651584;
    let data = dragon_curve_n(&data, n);
    println!("p2: {}", checksum(&data));
}
