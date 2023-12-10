pub struct Solution;

impl Solution {
    pub fn min_cost(costs: Vec<Vec<i32>>) -> i32 {
        let mut dp = vec![vec![0; 3]; costs.len() + 1];
        for i in 1..=costs.len() {
            dp[i][0] = dp[i - 1][1].min(dp[i - 1][2]) + costs[i - 1][0];
            dp[i][1] = dp[i - 1][0].min(dp[i - 1][2]) + costs[i - 1][1];
            dp[i][2] = dp[i - 1][0].min(dp[i - 1][1]) + costs[i - 1][2];
        }
        dp[costs.len()][0]
            .min(dp[costs.len()][1])
            .min(dp[costs.len()][2])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::min_cost(vec![vec![17, 2, 17], vec![16, 16, 5], vec![14, 3, 19]]),
            10
        );
        assert_eq!(Solution::min_cost(vec![vec![7, 6, 2]]), 2);
    }
}
