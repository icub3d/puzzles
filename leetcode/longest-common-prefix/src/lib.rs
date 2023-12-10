pub struct Solution;

impl Solution {
    pub fn longest_common_prefix(strs: Vec<String>) -> String {
        let mut result = String::new();
        let cc = strs
            .iter()
            .skip(1)
            .map(|s| s.chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>();
        for (i, c) in strs[0].chars().enumerate() {
            for ss in cc.iter() {
                if i >= ss.len() || ss[i] != c {
                    return result;
                }
            }
            result.push(c);
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
            Solution::longest_common_prefix(vec!["flower".into(), "flow".into(), "flight".into()]),
            "fl".to_string()
        );
        assert_eq!(
            Solution::longest_common_prefix(vec!["dog".into(), "racecar".into(), "car".into()]),
            "".to_string()
        );
    }
}
