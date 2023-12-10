pub struct Solution;

impl Solution {
    fn squared(mut n: usize) -> usize {
        let mut r = 0;
        while n > 0 {
            r += (n % 10) * (n % 10);
            n /= 10;
        }
        r
    }

    pub fn is_happy(n: i32) -> bool {
        use std::collections::HashSet;
        let mut seen = HashSet::new();
        let mut n = n as usize;
        loop {
            dbg!(n);
            if n == 1 {
                return true;
            }
            n = Solution::squared(n);
            if seen.contains(&n) {
                return false;
            }
            seen.insert(n);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn squared_works() {
        assert_eq!(Solution::squared(2), 4);
        assert_eq!(Solution::squared(19), 82);
        assert_eq!(Solution::squared(82), 68);
        assert_eq!(Solution::squared(68), 100);
        assert_eq!(Solution::squared(1), 1);
        assert_eq!(Solution::squared(16), 37);
        assert_eq!(Solution::squared(145), 42);
    }

    #[test]
    fn it_works() {
        assert_eq!(Solution::is_happy(19), true);
        assert_eq!(Solution::is_happy(2), false);
    }
}
