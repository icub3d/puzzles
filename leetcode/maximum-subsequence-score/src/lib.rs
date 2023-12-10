pub fn max_score(left: Vec<i32>, right: Vec<i32>, k: i32) -> i64 {
    let c = Combination::new(left.len(), k as usize);
    c.map(|c| {
        (c.iter().map(|&i| left[i]).sum::<i32>() as i64)
            * (c.iter().map(|&i| right[i]).min().unwrap() as i64)
    })
    .max()
    .unwrap()
}

struct Combination {
    n: usize,
    k: usize,
    i: usize,
}

impl Combination {
    /// Create a new combination iterator for an n-size array where k
    /// elements are taken from it each iteration.
    pub fn new(n: usize, k: usize) -> Combination {
        Combination { n, k, i: 0 }
    }
}

fn gray_code(n: usize) -> usize {
    return n ^ (n >> 1);
}

fn bits(n: usize) -> usize {
    let mut r = 0;
    let mut n = n;
    while n > 0 {
        r += n & 1;
        n >>= 1;
    }
    r
}

impl Iterator for Combination {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        // We use gray code to subtly modify a bitset:
        // https://en.wikipedia.org/wiki/Gray_code
        while self.i < (1 << self.n) {
            let cur = gray_code(self.i);
            self.i += 1;
            // We return any bitset that matches the request k size.
            if bits(cur) == self.k {
                let mut v = Vec::new();
                for j in 0..self.n {
                    if cur & (1 << j) != 0 {
                        v.push(j)
                    }
                }
                return Some(v);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(max_score(vec![1, 3, 3, 2], vec![2, 1, 3, 4], 3), 12);
        assert_eq!(max_score(vec![4, 2, 3, 1, 1], vec![7, 5, 10, 9, 6], 1), 30);
    }
}
