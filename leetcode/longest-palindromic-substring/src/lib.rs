pub struct Solution;

impl Solution {
    pub fn longest_palindrome(s: String) -> String {
        // Manacher's Algorithm
        // https://en.wikipedia.org/wiki/Longest_palindromic_substring
        let chars = "^"
            .chars()
            .chain(s.chars())
            .flat_map(|c| [c, '|'])
            .chain("$".chars())
            .collect::<Vec<char>>();

        let n = s.len();

        // Store the radius of the longest palindrome centered at i
        let mut p = vec![0; 2 * n + 1];
        p[2] = 1;

        let mut center = 2;
        let mut right = 3;
        let mut max = 1;
        let mut max_idx = 2;

        for i in 3..=2 * n {
            let mirror = 2 * center - i;

            // The previous may be a subset of this one.
            if i < right {
                p[i] = p[mirror].min(right - i);
            }

            // Try to increates the radius.
            while chars[i + p[i] + 1] == chars[i - p[i] - 1] {
                p[i] += 1;
            }

            // If we increased, the center and right will have changed.
            if i + p[i] > right {
                center = i;
                right = i + p[i];
            }

            // Check to see if we have a new max.
            if p[i] > max {
                max = p[i];
                max_idx = i;
            }
        }

        // The solution is surrounded by max_idx. This is just a fancy
        // way to pulling out the surrounding characters.
        chars
            .into_iter()
            .skip(max_idx - max + 1)
            .take(2 * max - 1)
            .filter(|u| *u != '|')
            .map(|u| u as char)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::longest_palindrome("babad".to_string()),
            "bab".to_string()
        );
        assert_eq!(
            Solution::longest_palindrome("cbbd".to_string()),
            "bb".to_string()
        );
    }
}
