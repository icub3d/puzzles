pub struct Solution;

impl Solution {
    pub fn longest_palindrome(words: Vec<String>) -> i32 {
        use std::collections::hash_map::Entry;
        use std::collections::HashMap;
        let mut matches = HashMap::new();
        let mut longest = 0;
        for word in words {
            let rev = word.chars().rev().collect::<String>();
            match matches.entry(rev) {
                Entry::Vacant(_) => {
                    matches.entry(word).and_modify(|v| *v += 1).or_insert(1);
                }
                Entry::Occupied(mut e) => {
                    longest += 4;
                    match *e.get() {
                        1 => {
                            e.remove();
                        }
                        _ => {
                            *e.get_mut() -= 1;
                        }
                    };
                }
            };
        }

        for (k, _) in matches.iter() {
            let k = k.chars().collect::<Vec<char>>();
            if k[0] == k[1] {
                longest += 2;
                break;
            }
        }
        longest as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::longest_palindrome(vec!["lc".into(), "cl".into(), "gg".into()]),
            6
        );
        assert_eq!(
            Solution::longest_palindrome(vec![
                "ab".into(),
                "ty".into(),
                "yt".into(),
                "lc".into(),
                "cl".into(),
                "ab".into()
            ]),
            8
        );
        assert_eq!(
            Solution::longest_palindrome(vec!["cc".into(), "ll".into(), "xx".into()]),
            2
        );
        assert_eq!(
            Solution::longest_palindrome(vec![
                "qo".into(),
                "fo".into(),
                "fq".into(),
                "qf".into(),
                "fo".into(),
                "ff".into(),
                "qq".into(),
                "qf".into(),
                "of".into(),
                "of".into(),
                "oo".into(),
                "of".into(),
                "of".into(),
                "qf".into(),
                "qf".into(),
                "of".into()
            ]),
            14
        );
    }
}
