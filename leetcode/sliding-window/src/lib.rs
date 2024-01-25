// This is meant to be an introductory problem. The question is, given
// a list of numbers and a number k, return the largest sum of k
// consecutive numbers in the list.
pub fn largest_continuous_sum(nums: Vec<isize>, k: usize) -> isize {
    // *state*
    //
    // In this case, it's just the max sum we've seen so far.
    let mut largest = std::isize::MIN;

    // *iterate*
    //
    // Both left and right will slide together, so we can use a range
    // + enumerate to get both values in our for loop.
    for (left, right) in (k..=nums.len()).enumerate() {
        // *update state*
        //
        // For this problem, we can simply sum the numbers in the
        // window and update our largest.
        let sum = nums[left..right].iter().sum();
        largest = largest.max(sum);
    }

    largest
}

// https://leetcode.com/problems/longest-substring-with-at-most-k-distinct-characters/description/
pub fn longest_substring_with_k_distinct_chars(s: &str, k: usize) -> usize {
    use std::collections::HashMap;

    // We'll use a vec of chars to simplify how the algorithm looks.
    let chars = s.chars().collect::<Vec<_>>();

    // *state*
    //
    // We want to track how many unique characters we have in our
    // window, but also how many of each so that as we slide the
    // window, we know when a character is no longer in the window at
    // all vs just having fewer of them.
    //
    // We also want to track our longest.
    let mut counts: HashMap<&char, usize> = HashMap::new();
    let mut longest = 0;

    // *iterate*
    //
    // In this case, left and right will slide independently, so we
    // need to track them separately.
    let mut left = 0;
    for (right, c) in chars.iter().enumerate() {
        // *update state*
        //
        // Increment our count for this char. Then slide the left side
        // of the window until we have k distinct chars. Once done, we
        // can update our longest value if we found a longer one.
        *counts.entry(c).or_default() += 1;

        while counts.len() > k {
            // We are sliding left, so we want to decrement it's count
            // and then remove it from the map it it's now zero.
            let left_char = chars[left];
            *counts.get_mut(&left_char).unwrap() -= 1;
            if counts[&left_char] == 0 {
                counts.remove(&left_char);
            }
            left += 1;
        }

        longest = longest.max(right - left + 1);
    }
    longest
}

// https://leetcode.com/problems/minimum-window-substring/description/
pub fn min_window_substring(s: &str, t: &str) -> String {
    use std::collections::HashMap;

    // Again, we'll use a vec of chars to simplify how the algorithm
    // looks.
    let chars = s.chars().collect::<Vec<_>>();

    // Handle the edge cases that leetcode presented.
    if s.is_empty() || t.is_empty() {
        return "".to_string();
    }

    // *state*
    //
    // We want to track how many of each char we have left to find in
    // t. If all of them are zero or less, we know we have a valid
    // window. We also want to track our min_window.
    let mut counts: HashMap<char, isize> = t.chars().fold(HashMap::new(), |mut counts, c| {
        *counts.entry(c).or_default() += 1;
        counts
    });
    let mut min_window = "";

    // *iterate*
    //
    // Again, left and right will slide independently.
    let mut left = 0;
    for (right, c) in chars.iter().enumerate() {
        // *update state*
        //
        // The general idea is that we want to decrement values in t
        // until we have them all. And then as long we we have all of
        // the values in t, we can keep sliding left.
        if let Some(count) = counts.get_mut(c) {
            *count -= 1
        }

        // We want to slide left as long a we have a valid window.
        while counts.values().all(|&count| count <= 0) {
            // If we have a new minumum window, update it.
            if right - left + 1 < min_window.len() || min_window.is_empty() {
                min_window = &s[left..=right];
            }

            // If left is in our map, increment it's count and then shift left.
            if let Some(count) = counts.get_mut(&chars[left]) {
                *count += 1;
            }
            left += 1;
        }
    }

    min_window.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_largest_continuous_sum() {
        let nums = vec![1, 2, 3, 4, 5];
        let k = 3;
        assert_eq!(largest_continuous_sum(nums, k), 12);

        let nums = vec![1, 2, 3, -4, 5];
        let k = 3;
        assert_eq!(largest_continuous_sum(nums, k), 6);
    }

    #[test]
    fn test_longest_substring_with_k_distinct_chars() {
        let s = "araaci";
        let k = 2;
        assert_eq!(longest_substring_with_k_distinct_chars(s, k), 4);

        let s = "araaci";
        let k = 1;
        assert_eq!(longest_substring_with_k_distinct_chars(s, k), 2);

        let s = "cbbebi";
        let k = 3;
        assert_eq!(longest_substring_with_k_distinct_chars(s, k), 5);
    }

    #[test]
    fn test_min_window_substring() {
        let s = "ADOBECODEBANC";
        let t = "ABC";
        assert_eq!(min_window_substring(s, t), "BANC");

        let s = "a";
        let t = "a";
        assert_eq!(min_window_substring(s, t), "a");

        let s = "a";
        let t = "aa";
        assert_eq!(min_window_substring(s, t), "");

        let s = "a";
        let t = "b";
        assert_eq!(min_window_substring(s, t), "");

        let s = "aa";
        let t = "aa";
        assert_eq!(min_window_substring(s, t), "aa");

        let s = "bbaa";
        let t = "aba";
        assert_eq!(min_window_substring(s, t), "baa");

        let s = "bbaa";
        let t = "abb";
        assert_eq!(min_window_substring(s, t), "bba");

        let s = "bbaa";
        let t = "abaaa";
        assert_eq!(min_window_substring(s, t), "");
    }
}
