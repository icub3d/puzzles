pub struct Solution;

impl Solution {
    pub fn word_break(s: String, word_dict: Vec<String>) -> bool {
        // dp table to store our word break results. dp[i] is true if
        // s[0..i] can be segmented into words from the dictionary.
        let mut dp = vec![false; s.len() + 1];
        dp[0] = true; // the empty string is valid.
        for i in 1..=s.len() {
            // If we can find a word in our dictionary that ends at i
            // and the character before it is also a valid word break,
            // then we have a valid word break at i.
            for word in &word_dict {
                if i >= word.len() && dp[i - word.len()] && &s[i - word.len()..i] == word {
                    dp[i] = true;
                    break;
                }
            }
        }
        // we found a valid word break is the last element is true.
        dp[s.len()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::word_break(
                "leetcode".to_string(),
                vec!["leet".to_string(), "code".to_string()]
            ),
            true
        );
        assert_eq!(
            Solution::word_break(
                "applepenapple".to_string(),
                vec!["apple".to_string(), "pen".to_string()]
            ),
            true
        );
        assert_eq!(
            Solution::word_break(
                "catsandog".to_string(),
                vec![
                    "cats".to_string(),
                    "dog".to_string(),
                    "sand".to_string(),
                    "and".to_string(),
                    "cat".to_string()
                ]
            ),
            false
        );
    }
}
