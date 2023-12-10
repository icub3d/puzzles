pub struct Solution;

impl Solution {
    pub fn max_profit(k: i32, prices: Vec<i32>) -> i32 {
        let k = k as usize;
        let n = prices.len();
        if n == 0 || k == 0 {
            return 0;
        }

        // This is a 3D array where each day/price has a the value for
        // buying and selling on that day based on the numbers of
        // boughts and solds.
        let mut dp = vec![vec![vec![-1_000_000_000; 2]; k + 1]; n];

        dp[0][0][0] = 0;
        dp[0][1][1] = -prices[0]; // It wouldn't make sense to sell on first day so make it feel bad.

        for (i, p) in prices.iter().enumerate().skip(1) {
            for j in 0..k + 1 {
                dp[i][j][0] = std::cmp::max(dp[i - 1][j][0], dp[i - 1][j][1] + p);
                if j > 0 {
                    dp[i][j][1] = std::cmp::max(dp[i - 1][j][1], dp[i - 1][j - 1][0] - p);
                }
            }
        }

        dp[n - 1].iter().map(|x| x[0]).max().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::max_profit(2, vec![2, 4, 1]), 2);
        assert_eq!(Solution::max_profit(2, vec![3, 2, 6, 5, 0, 3]), 7);
        assert_eq!(Solution::max_profit(2, vec![3, 3, 5, 0, 0, 3, 1, 4]), 6);
    }
}
