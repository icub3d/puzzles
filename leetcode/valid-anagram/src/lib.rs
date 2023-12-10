pub struct Solution;

impl Solution {
    pub fn is_anagram(s: String, t: String) -> bool {
        use std::collections::hash_map::Entry;
        use std::collections::HashMap;
        if s.len() != t.len() {
            return false;
        }
        let mut m = s.chars().fold(HashMap::new(), |mut m, c| {
            m.entry(c).and_modify(|v| *v += 1).or_insert(1);
            m
        });
        for c in t.chars() {
            match m.entry(c) {
                Entry::Occupied(mut e) => match *e.get() == 1 {
                    true => {
                        e.remove_entry();
                    }
                    false => {
                        e.insert(*e.get() - 1);
                    }
                },
                Entry::Vacant(_) => return false,
            };
        }
        m.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::is_anagram("anagram".into(), "nagaram".into()),
            true
        );
        assert_eq!(Solution::is_anagram("rat".into(), "car".into()), false);
    }
}
