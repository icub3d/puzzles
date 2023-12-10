pub struct Solution;

impl Solution {
    pub fn min_distance(word1: String, word2: String) -> i32 {
        let word1 = word1.chars().collect::<Vec<char>>();
        let word2 = word2.chars().collect::<Vec<char>>();

        if word1.len() == 0 {
            return word2.len() as i32;
        }

        if word2.len() == 0 {
            return word1.len() as i32;
        }

        // Setup our dp table.
        let mut dp = vec![vec![0 as i32; word2.len() + 1]; word1.len() + 1];
        for i in 1..=word1.len() {
            dp[i][0] = i as i32;
        }
        for j in 1..=word2.len() {
            dp[0][j] = j as i32;
        }

        for i in 1..=word1.len() {
            for j in 1..=word2.len() {
                if word1[i - 1] == word2[j - 1] {
                    dp[i][j] = dp[i - 1][j - 1];
                } else {
                    dp[i][j] = 1 + vec![dp[i - 1][j], dp[i][j - 1], dp[i - 1][j - 1]]
                        .iter()
                        .min()
                        .unwrap();
                }
            }
        }
        dp[word1.len()][word2.len()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::min_distance("Saturday".to_string(), "Sunday".to_string()),
            3
        );
        assert_eq!(Solution::min_distance("".to_string(), "".to_string()), 0);
        assert_eq!(Solution::min_distance("".to_string(), "a".to_string()), 1);
        assert_eq!(Solution::min_distance("a".to_string(), "".to_string()), 1);
        assert_eq!(
            Solution::min_distance("horse".to_string(), "ros".to_string()),
            3
        );
        assert_eq!(
            Solution::min_distance("intention".to_string(), "execution".to_string()),
            5
        );
    }
}
