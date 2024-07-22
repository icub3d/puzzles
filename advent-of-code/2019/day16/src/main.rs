const INPUT: &str = include_str!("../input");

fn fft(signal: &[isize]) -> Vec<isize> {
    let mut output = Vec::with_capacity(signal.len());
    for i in 0..signal.len() {
        let pattern = [0, 1, 0, -1]
            .iter()
            .flat_map(|&x| std::iter::repeat(x).take(i + 1))
            .cycle()
            .skip(1);
        let sum = signal
            .iter()
            .zip(pattern)
            .map(|(digit, pattern)| digit * pattern)
            .sum::<isize>();
        output.push(sum.abs() % 10);
    }
    output
}

fn main() {
    let signal = INPUT
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as isize)
        .collect::<Vec<_>>();

    // println!("signal: {:?}", signal.len());
    // let signal = "80871224585914546619083218645595"
    //     .chars()
    //     .map(|c| c.to_digit(10).unwrap() as isize)
    //     .collect::<Vec<_>>();
    let now = std::time::Instant::now();
    let mut p1_signal = signal.clone();
    for _ in 0..100 {
        p1_signal = fft(&p1_signal);
    }
    let p1 = &p1_signal[..8].iter().fold(0, |acc, x| acc * 10 + x);
    println!("p1: {} ({:?})", p1, now.elapsed());

    let now = std::time::Instant::now();
    let p2_signal = signal.clone();
    let start = p2_signal[..7].iter().fold(0, |acc, x| acc * 10 + x) as usize;
    let end = p2_signal.len() * 10_000;
    let mut cur = Vec::new();
    for i in start..end {
        cur.push(p2_signal[i % p2_signal.len()]);
    }
    for _ in 0..100 {
        let mut sums = Vec::new();
        let mut total = 0;
        sums.push(0);
        for c in cur.iter() {
            total += c;
            sums.push(total);
        }

        for i in 0..cur.len() {
            let value = sums.last().unwrap() - sums[i];
            cur[i] = value.abs() % 10;
        }
    }
    let p2 = &cur[..8].iter().fold(0, |acc, x| acc * 10 + x);

    println!("p2: {} ({:?})", p2, now.elapsed());
}
