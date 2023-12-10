pub struct Solution;

impl Solution {
    pub fn is_valid(s: String) -> bool {
        use std::collections::HashMap;
        let mut stack = vec![];
        let matches: HashMap<char, char> = vec![('(', ')'), ('[', ']'), ('{', '}')]
            .into_iter()
            .collect();
        for c in s.chars() {
            match c {
                '[' | '{' | '(' => stack.push(c),
                ']' | '}' | ')' => match stack.pop() {
                    None => return false,
                    Some(e) => match matches[&e] == c {
                        false => return false,
                        _ => (),
                    },
                },
                _ => return false,
            };
        }
        stack.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::is_valid("{}".into()), true);
        assert_eq!(Solution::is_valid("[]{}[]".into()), true);
        assert_eq!(Solution::is_valid("[{}{}{}{[]([][])}]{}[]".into()), true);
        assert_eq!(Solution::is_valid("[{}{}{}{]([][])}]{}[]".into()), false);
    }
}
