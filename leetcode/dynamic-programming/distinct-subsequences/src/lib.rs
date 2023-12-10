pub struct Solution;

impl Solution {
    pub fn num_distinct(s: String, t: String) -> i32 {
        let mut dp = vec![vec![0; t.len() + 1]; s.len() + 1];
        let s = s.as_bytes();
        let t = t.as_bytes();

        // All empty strings are subsequence of each other
        for i in 0..=s.len() {
            dp[i][0] = 1;
        }

        for i in 1..=s.len() {
            for j in 1..=t.len() {
                if s[i - 1] == t[j - 1] {
                    // If they are the same, we sum up keeping vs deleting.
                    dp[i][j] = dp[i - 1][j - 1] + dp[i - 1][j];
                } else {
                    dp[i][j] = dp[i - 1][j];
                }
            }
        }
        dp[s.len()][t.len()] as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::num_distinct("rabbbit".to_string(), "rabbit".to_string()),
            3
        );
        assert_eq!(
            Solution::num_distinct("babgbag".to_string(), "bag".to_string()),
            5
        );
    }
}
