pub fn length_of_longest_substring(s: String) -> i32 {
    use std::cmp::max;
    use std::collections::HashSet;
    let s = s.chars().collect::<Vec<char>>();
    let mut longest = 0;
    let mut left = 0;
    let mut characters: HashSet<char> = HashSet::new();
    for right in 0..s.len() {
        let r = s[right];
        while left <= right && characters.contains(&r) {
            characters.remove(&s[left]);
            left += 1;
        }
        characters.insert(r);
        longest = max(longest, right - left + 1);
    }
    longest as i32
}
#[cfg(test)]
mod tests {
    use super::length_of_longest_substring;
    #[test]
    fn it_works() {
        assert_eq!(length_of_longest_substring("abcabcbb".to_string()), 3);
        assert_eq!(length_of_longest_substring("bbbbb".to_string()), 1);
        assert_eq!(length_of_longest_substring("pwwkew".to_string()), 3);
    }
}
