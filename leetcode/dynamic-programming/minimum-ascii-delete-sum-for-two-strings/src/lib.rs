pub struct Solution;

impl Solution {
    pub fn minimum_delete_sum(s1: String, s2: String) -> i32 {
        let mut dp = vec![vec![0; s2.len() + 1]; s1.len() + 1];
        let s1_bytes = s1.bytes().collect::<Vec<u8>>();
        let s2_bytes = s2.bytes().collect::<Vec<u8>>();

        for i in (0..s1.len()).rev() {
            dp[i][s2.len()] = dp[i + 1][s2.len()] + s1_bytes[i] as i32;
        }
        for j in (0..s2.len()).rev() {
            dp[s1.len()][j] = dp[s1.len()][j + 1] + s2_bytes[j] as i32;
        }

        for i in (0..s1.len()).rev() {
            for j in (0..s2.len()).rev() {
                if s1_bytes[i] == s2_bytes[j] {
                    dp[i][j] = dp[i + 1][j + 1];
                } else {
                    dp[i][j] = std::cmp::min(
                        dp[i + 1][j] + s1_bytes[i] as i32,
                        dp[i][j + 1] + s2_bytes[j] as i32,
                    );
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
            Solution::minimum_delete_sum("sea".to_string(), "eat".to_string()),
            231
        );
        assert_eq!(
            Solution::minimum_delete_sum("delete".to_string(), "leet".to_string()),
            403
        );
    }
	
    #[test]
    fn test_empty_strings() {
        let s1 = "";
        let s2 = "";
        assert_eq!(Solution::minimum_delete_sum(String::from(s1), String::from(s2)), 0);
    }

    #[test]
    fn test_identical_strings() {
        let s1 = "abc";
        let s2 = "abc";
        assert_eq!(Solution::minimum_delete_sum(String::from(s1), String::from(s2)), 0);
    }

    #[test]
    fn test_different_strings() {
        let s1 = "abc";
        let s2 = "def";
        assert_eq!(Solution::minimum_delete_sum(String::from(s1), String::from(s2)), 597);
    }

    #[test]
    fn test_different_lengths() {
        let s1 = "abcde";
        let s2 = "bc";
        assert_eq!(Solution::minimum_delete_sum(String::from(s1), String::from(s2)), 298);
    }

    #[test]
    fn test_same_characters_different_order() {
        let s1 = "abcd";
        let s2 = "dcba";
        assert_eq!(Solution::minimum_delete_sum(String::from(s1), String::from(s2)), 588);
    }

    #[test]
    fn test_very_long_strings() {
        let s1 = "a".repeat(1000);
        let s2 = "b".repeat(1000);
        assert_eq!(Solution::minimum_delete_sum(s1, s2), 195000);
    }
}
