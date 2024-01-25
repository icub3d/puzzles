fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let lines = input.lines().collect::<Vec<_>>();
    let p1 = lines.iter().map(|line| decompress(line)).sum::<usize>();
    println!("p1: {}", p1);

    let p2 = lines
        .iter()
        .map(|line| decompress_recursive(line))
        .sum::<usize>();
    println!("p2: {}", p2);
}

fn decompress_recursive(input: &str) -> usize {
    let mut len = 0;

    let mut chars = input.chars();
    while let Some(c) = chars.next() {
        if c == '(' {
            let mut marker = String::new();
            for c in chars.by_ref().take_while(|c| *c != ')') {
                marker.push(c);
            }
            let mut marker = marker.split('x');
            let length = marker.next().unwrap().parse::<usize>().unwrap();
            let subset = chars.by_ref().take(length).collect::<String>();
            let subset_len = decompress_recursive(&subset);
            let times = marker.next().unwrap().parse::<usize>().unwrap();
            len += subset_len * times;
        } else {
            len += 1;
        }
    }
    len
}

fn decompress(input: &str) -> usize {
    let mut len = 0;

    let mut chars = input.chars();
    while let Some(c) = chars.next() {
        if c == '(' {
            let mut marker = String::new();
            for c in chars.by_ref().take_while(|c| *c != ')') {
                marker.push(c);
            }
            let mut marker = marker.split('x');
            let length = marker.next().unwrap().parse::<usize>().unwrap();
            chars.by_ref().take(length).for_each(|_| ());
            let times = marker.next().unwrap().parse::<usize>().unwrap();
            len += length * times;
        } else {
            len += 1;
        }
    }
    len
}
