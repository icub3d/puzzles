pub struct Solution;

impl Solution {
    pub fn remove_vowels(s: String) -> String {
        let mut result = String::new();
        for c in s.chars() {
            if c != 'a' && c != 'e' && c != 'i' && c != 'o' && c != 'u' {
                result.push(c);
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::remove_vowels("leetcodeisacommunityforcoders".to_string()),
            "ltcdscmmntyfrcdrs".to_string()
        );
        assert_eq!(Solution::remove_vowels("aeiou".to_string()), "".to_string());
    }
}
