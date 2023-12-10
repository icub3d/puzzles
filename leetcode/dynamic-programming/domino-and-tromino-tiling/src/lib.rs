pub struct Solution;

impl Solution {
    pub fn num_tilings(n: i32) -> i32 {
        const MOD: i64 = 1_000_000_007;

        if n <= 2 {
            return n;
        }

        let mut fp: i64 = 1;
        let mut f: i64 = 2;
        let mut p: i64 = 1;

        for _ in 3..=n {
            let tmp = f;
            f = (f + fp + 2 * p) % MOD;
            p = (p + fp) % MOD;
            fp = tmp;
        }
        f as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::num_tilings(3), 5);
        assert_eq!(Solution::num_tilings(4), 11);
        assert_eq!(Solution::num_tilings(30), 312342182);
    }
}
