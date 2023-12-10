pub struct Solution;

impl Solution {
    pub fn num_decodings(s: String) -> i32 {
        let mut dp = vec![0; s.len() + 1];
        dp[0] = 1;
        for (i, c) in s.chars().enumerate() {
            if c != '0' {
                dp[i + 1] += dp[i];
            }
            if i > 0 {
                let n = s[i - 1..i + 1].parse::<i32>().unwrap();
                if (10..=26).contains(&n) {
                    dp[i + 1] += dp[i - 1];
                }
            }
        }
        dp[s.len()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::num_decodings("12".to_string()), 2);
        assert_eq!(Solution::num_decodings("226".to_string()), 3);
        assert_eq!(Solution::num_decodings("0".to_string()), 0);
        assert_eq!(Solution::num_decodings("06".to_string()), 0);
    }
}
