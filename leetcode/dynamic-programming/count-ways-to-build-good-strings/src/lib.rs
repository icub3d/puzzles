pub struct Solution;

impl Solution {
    pub fn count_good_strings(low: i32, high: i32, zero: i32, one: i32) -> i32 {
        let mut dp = vec![0; high as usize + 1];
        dp[0] = 1;
        let m = 10_i32.pow(9) + 7;

        for end in 1..=high {
            if end >= zero {
                dp[end as usize] += dp[(end - zero) as usize];
            }
            if end >= one {
                dp[end as usize] += dp[(end - one) as usize];
            }
            dp[end as usize] %= m;
        }

        (dp[low as usize..=high as usize]
            .iter()
            .map(|i| *i as u64)
            .sum::<u64>()
            % m as u64) as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::count_good_strings(3, 3, 1, 1), 8);
        assert_eq!(Solution::count_good_strings(2, 3, 1, 2), 5);
        assert_eq!(Solution::count_good_strings(1, 100000, 1, 1), 215447031);
    }
}
