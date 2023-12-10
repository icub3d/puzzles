pub struct Solution;

impl Solution {
    pub fn longest_common_subsequence(text1: String, text2: String) -> i32 {
        let text1 = text1.chars().collect::<Vec<_>>();
        let text2 = text2.chars().collect::<Vec<_>>();
        let mut dp = vec![vec![0; text2.len() + 1]; text1.len() + 1];
        for i in (0..text1.len()).rev() {
            for j in (0..text2.len()).rev() {
                if text1[i] == text2[j] {
                    dp[i][j] = dp[i + 1][j + 1] + 1;
                } else {
                    dp[i][j] = dp[i + 1][j].max(dp[i][j + 1]);
                }
            }
        }
        dp[0][0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::longest_common_subsequence("abcde".to_string(), "ace".to_string()),
            3
        );
        assert_eq!(
            Solution::longest_common_subsequence("abc".to_string(), "abc".to_string()),
            3
        );
        assert_eq!(
            Solution::longest_common_subsequence("abc".to_string(), "def".to_string()),
            0
        );
    }
}
