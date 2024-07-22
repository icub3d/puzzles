fn main() {
    let input = include_str!("../input");

    // Part 1
    let lengths = input
        .trim()
        .split(',')
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    println!("p1: {}", run(256, lengths.clone()));

    // Part 2
    println!("p2: {}", knot_hash(input.trim()));
}

fn knot_hash(input: &str) -> String {
    let mut lengths = input.bytes().map(|x| x as usize).collect::<Vec<usize>>();
    lengths.extend_from_slice(&[17, 31, 73, 47, 23]);

    let mut list = (0..256).collect::<Vec<usize>>();
    let mut pos = 0;
    let mut skip = 0;

    for _ in 0..64 {
        for length in lengths.iter() {
            let mut i = pos;
            let mut j = (pos + length - 1) % 256;
            for _ in 0..*length / 2 {
                list.swap(i, j);
                i = (i + 1) % 256;
                j = (j + 256 - 1) % 256;
            }
            pos = (pos + length + skip) % 256;
            skip += 1;
        }
    }

    list.chunks(16)
        .map(|chunk| chunk.iter().fold(0, |acc, x| acc ^ x))
        .fold(String::new(), |acc, x| acc + &format!("{:02x}", x))
}

fn run(size: usize, lengths: Vec<usize>) -> usize {
    let mut list = (0..size).collect::<Vec<usize>>();
    let mut pos = 0;

    for (skip, length) in lengths.into_iter().enumerate() {
        let mut i = pos;
        let mut j = (pos + length - 1) % size;
        for _ in 0..length / 2 {
            list.swap(i, j);
            i = (i + 1) % size;
            j = (j + size - 1) % size;
        }
        pos = (pos + length + skip) % size;
    }

    list[0] * list[1]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
        assert_eq!(run(5, vec![3, 4, 1, 5]), 12);
    }
}
