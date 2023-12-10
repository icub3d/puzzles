pub struct Solution;

impl Solution {
    pub fn min_insertions(s: String) -> i32 {
        // The intuition here is that the solution is the difference
        // between the length of s and the longest common subsequences
        // of s and its reverse.
        let r = s.chars().rev().collect::<Vec<_>>();
        let s = s.chars().collect::<Vec<_>>();
        let mut dp = vec![vec![0; s.len() + 1]; s.len() + 1];
        for i in (0..s.len()).rev() {
            for j in (0..r.len()).rev() {
                if s[i] == r[j] {
                    dp[i][j] = dp[i + 1][j + 1] + 1;
                } else {
                    dp[i][j] = dp[i + 1][j].max(dp[i][j + 1]);
                }
            }
        }
        (s.len() - dp[0][0]) as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::min_insertions("".to_string()), 0);
        assert_eq!(Solution::min_insertions("zzazz".to_string()), 0);
        assert_eq!(Solution::min_insertions("mbadm".to_string()), 2);
        assert_eq!(Solution::min_insertions("leetcode".to_string()), 5);
    }
}
