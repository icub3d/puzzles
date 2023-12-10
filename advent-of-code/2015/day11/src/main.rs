fn main() {
    let mut input = "cqjxjnds".chars().map(|c| c as u8).collect::<Vec<_>>();

    while !is_valid(&input) {
        increment(&mut input);
    }
    println!("Part 1: {}", String::from_utf8(input.clone()).unwrap());

    increment(&mut input);
    while !is_valid(&input) {
        increment(&mut input);
    }
    println!("Part 2: {}", String::from_utf8(input.clone()).unwrap());
}

fn increment(input: &mut [u8]) {
    let mut i = input.len() - 1;
    loop {
        if input[i] == b'z' {
            input[i] = b'a';
            i -= 1;
        } else {
            input[i] = input[i] + 1;
            break;
        }
    }
}

fn is_valid(input: &[u8]) -> bool {
    let mut increasing = false;
    let mut pairs = vec![];

    for (i, c) in input.iter().enumerate() {
        if *c == b'i' || *c == b'o' || *c == b'l' {
            return false;
        }

        if i > 2 {
            if input[i - 2] == input[i - 1] - 1 && input[i - 1] == *c - 1 {
                increasing = true;
            }
        }

        if i > 0 && pairs.len() < 2 && input[i - 1] == *c {
            if pairs.len() == 0 {
                pairs.push(i - 1);
            } else if pairs[0] + 1 != i - 1 {
                pairs.push(i - 1);
            }
        }
    }

    return increasing && pairs.len() == 2;
}
