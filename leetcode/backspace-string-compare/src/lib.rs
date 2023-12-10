pub struct Solution;

impl Solution {
    pub fn backspace_compare(s: String, t: String) -> bool {
        let mut ss = vec![];
        for c in s.chars() {
            if c == '#' {
                if ss.len() > 0 {
                    ss.pop();
                }
                continue;
            }
            ss.push(c);
        }
        let mut tt = vec![];
        for c in t.chars() {
            if c == '#' {
                if tt.len() > 0 {
                    tt.pop();
                }
                continue;
            }
            tt.push(c);
        }
        ss == tt
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::backspace_compare("ab#c".into(), "ad#c".into()),
            true
        );
        assert_eq!(
            Solution::backspace_compare("ab##".into(), "c#d#".into()),
            true
        );
        assert_eq!(Solution::backspace_compare("a#c".into(), "b".into()), false);
    }
}
