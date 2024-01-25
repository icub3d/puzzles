use std::collections::HashSet;

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let input = input.lines().collect::<Vec<_>>();
    println!(
        "p1: {:?}",
        input
            .iter()
            .filter(|&x| support_tls(&x.chars().collect::<Vec<char>>()))
            .count()
    );
    println!(
        "p2: {:?}",
        input
            .iter()
            .filter(|&x| support_ssl(&x.chars().collect::<Vec<char>>()))
            .count()
    );
}

fn support_ssl(input: &[char]) -> bool {
    let mut hypernet = false;
    let mut abas = HashSet::new();
    let mut babs = HashSet::new();
    let mut left = 0;
    let mut right = 2;
    while right < input.len() {
        if input[right] == '[' {
            hypernet = true;
            left = right + 1;
            right = left + 2;
            continue;
        } else if input[left] == ']' {
            hypernet = false;
        } else if input[left] == input[right] && input[left] != input[left + 1] {
            if hypernet {
                babs.insert((input[left + 1], input[left]));
            } else {
                abas.insert((input[left], input[left + 1]));
            }
        }
        left += 1;
        right += 1;
    }
    abas.intersection(&babs).count() > 0
}

fn support_tls(input: &[char]) -> bool {
    let mut hypernet = false;
    let mut abba = false;
    let mut left = 0;
    let mut right = 3;
    while right < input.len() {
        if input[right] == '[' {
            hypernet = true;
            left = right + 1;
            right = left + 3;
            continue;
        } else if input[left] == ']' {
            hypernet = false;
        } else if input[left] == input[left + 3]
            && input[left + 1] == input[left + 2]
            && input[left] != input[left + 1]
        {
            if hypernet {
                return false;
            }
            abba = true;
        }
        left += 1;
        right += 1;
    }
    abba
}
