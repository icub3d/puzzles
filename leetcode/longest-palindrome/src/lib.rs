pub struct Solution;

impl Solution {
    pub fn longest_palindrome(s: String) -> i32 {
        use std::collections::HashMap;
        let mut matches = HashMap::new();
        let mut longest = 0;
        for c in s.chars() {
            let count = *matches.entry(c).and_modify(|v| *v += 1).or_insert(1);
            if count % 2 == 0 {
                longest += 2;
            }
        }

        // See if we have a single.
        for (_, v) in matches.iter() {
            if v % 2 == 1 {
                longest += 1;
                break;
            }
        }

        longest
    }
}

#[cfg(test)]
mod tests {
    use super::Solution;

    #[test]
    fn it_works() {
        assert_eq!(Solution::longest_palindrome("abccccdd".into()), 7);
        assert_eq!(Solution::longest_palindrome("a".into()), 1);
    }
}
