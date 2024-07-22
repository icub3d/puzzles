fn msb(n: u32) -> u32 {
    let mut n = n;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n -= n >> 1;
    n
}

fn josephus(n: u32) -> u32 {
    let m = msb(n);
    2 * (n - m) + 1
}

fn p2(n: u32) -> u32 {
    let p = ((n as f32).ln() / (3.0f32).ln()).floor() as u32;
    let b = 3u32.pow(p);
    if b == n {
        n
    } else if n - b <= b {
        n - b
    } else {
        2 * n - 3 * b
    }
}

fn main() {
    let n = 3014387;
    println!("p1: {}", josephus(n));
    println!("p2: {}", p2(n));
}
