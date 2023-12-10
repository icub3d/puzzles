// https://leetcode.com/problems/longest-repeating-character-replacement/
pub fn character_replacement(s: String, k: i32) -> i32 {
    use std::cmp;
    use std::collections::HashMap;

    // This is more rust stuff, we turn the string to an int and use usize to minimize casting.
    let k: usize = k as usize;
    let chars = s.chars().collect::<Vec<char>>();

    let mut chars_in_window = HashMap::new();
    let mut left: usize = 0;
    let mut most_frequent: usize = 0;
    let mut longest = 0;
    for (right, cur) in chars.iter().enumerate() {
        let cur_count = *chars_in_window
            .entry(cur)
            .and_modify(|count| *count += 1)
            .or_insert(1);

        most_frequent = cmp::max(most_frequent, cur_count);

        while right - left + 1 - most_frequent > k {
            chars_in_window
                .entry(&chars[left])
                .and_modify(|count| *count -= 1);
            left += 1;
        }

        longest = cmp::max(longest, right - left + 1);
    }
    longest as i32
}

#[cfg(test)]
mod tests {
    use super::character_replacement;
    #[test]
    fn it_works() {
        assert_eq!(character_replacement("ABAB".to_string(), 2), 4);
        assert_eq!(character_replacement("AABABBA".to_string(), 1), 4);
    }
}
