pub struct Solution;

impl Solution {
    pub fn first_uniq_char(s: String) -> i32 {
        use std::collections::HashMap;
        let m = s.chars().enumerate().fold(
            HashMap::new(),
            |mut m: HashMap<char, (usize, usize)>, (i, c)| {
                m.entry(c).and_modify(|e| e.1 += 1).or_insert((i, 1));
                m
            },
        );
        match m.iter().filter(|v| v.1 .1 == 1).map(|v| v.1 .0).min() {
            Some(i) => i as i32,
            _ => -1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::first_uniq_char("leetcode".into()), 0);
        assert_eq!(Solution::first_uniq_char("loveleetcode".into()), 2);
        assert_eq!(Solution::first_uniq_char("aabb".into()), -1);
    }
}
