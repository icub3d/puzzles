pub fn is_subsequence(s: String, t: String) -> bool {
    helper(s.as_bytes(), t.as_bytes())
}

pub fn helper(ss: &[u8], tt: &[u8]) -> bool {
    if ss.len() == 0 {
        return true;
    }
    for (i, t) in tt.iter().enumerate() {
        if *t == ss[0] {
            return helper(&ss[1..], &tt[i + 1..]);
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            is_subsequence("aaaaaa".to_string(), "bbaaaa".to_string()),
            false
        );
        assert_eq!(
            is_subsequence("abc".to_string(), "ahbgdc".to_string()),
            true
        );
        assert_eq!(
            is_subsequence("axc".to_string(), "ahbgdc".to_string()),
            false
        );
    }
}
