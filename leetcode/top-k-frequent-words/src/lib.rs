pub struct Solution;

impl Solution {
    pub fn top_k_frequent(words: Vec<String>, k: i32) -> Vec<String> {
        use std::collections::BinaryHeap;
        use std::collections::HashMap;

        let mut m = HashMap::new();
        for word in words.iter() {
            m.entry(word).and_modify(|v| *v += 1).or_insert(1);
        }

        let mut h = BinaryHeap::new();
        for (word, count) in m.iter() {
            h.push(Frequency {
                count: *count,
                word: word.to_string(),
            });
        }
        let mut v = vec![];
        while h.len() > 0 && v.len() < k as usize {
            v.push(h.pop().unwrap().word);
        }
        v
    }
}

pub struct Frequency {
    count: usize,
    word: String,
}

use std::cmp::Ordering;
use std::cmp::Ordering::{Equal, Greater, Less};
impl Ord for Frequency {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.count.cmp(&other.count) {
            Less => return Less,
            Greater => Greater,
            Equal => match self.word.cmp(&other.word) {
                Less => return Greater,
                Greater => return Less,
                Equal => return Equal,
            },
        }
    }
}

impl PartialOrd for Frequency {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Frequency {
    fn eq(&self, other: &Self) -> bool {
        self.count == other.count && self.word == other.word
    }
}
impl Eq for Frequency {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::top_k_frequent(
                vec![
                    "i".into(),
                    "love".into(),
                    "leetcode".into(),
                    "i".into(),
                    "love".into(),
                    "coding".into()
                ],
                2
            ),
            vec!["i".to_string(), "love".to_string()]
        );
        assert_eq!(
            Solution::top_k_frequent(
                vec![
                    "the".into(),
                    "day".into(),
                    "is".into(),
                    "sunny".into(),
                    "the".into(),
                    "the".into(),
                    "the".into(),
                    "sunny".into(),
                    "is".into(),
                    "is".into()
                ],
                4
            ),
            vec![
                "the".to_string(),
                "is".to_string(),
                "sunny".to_string(),
                "day".to_string()
            ]
        );
    }
}
