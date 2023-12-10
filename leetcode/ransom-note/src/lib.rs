pub struct Solution;

impl Solution {
    pub fn can_construct(ransom_note: String, magazine: String) -> bool {
        use std::collections::HashMap;
        let r = ransom_note.chars().fold(HashMap::new(), |mut m, c| {
            m.entry(c).and_modify(|v| *v += 1).or_insert(1);
            m
        });
        let m = magazine.chars().fold(HashMap::new(), |mut m, c| {
            m.entry(c).and_modify(|v| *v += 1).or_insert(1);
            m
        });

        for (k, v) in r.iter() {
            match m.get(k) {
                Some(n) => {
                    if v > n {
                        return false;
                    }
                }
                _ => return false,
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::can_construct("a".into(), "b".into()), false);
        assert_eq!(Solution::can_construct("aa".into(), "ab".into()), false);
        assert_eq!(Solution::can_construct("aa".into(), "baa".into()), true);
    }
}
