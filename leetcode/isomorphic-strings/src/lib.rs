pub fn is_isomorphic(s: String, t: String) -> bool {
    use std::collections::HashMap;
    let mut ltor = HashMap::new();
    let mut rtol = HashMap::new();
    for (l, r) in s.chars().zip(t.chars()) {
        let lv = ltor.entry(l).or_insert(r);
        let rv = rtol.entry(r).or_insert(l);
        if *rv != l || *lv != r {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(is_isomorphic("egg".to_string(), "add".to_string()), true);
        assert_eq!(is_isomorphic("badc".to_string(), "baba".to_string()), false);
        assert_eq!(is_isomorphic("foo".to_string(), "bar".to_string()), false);
        assert_eq!(
            is_isomorphic("paper".to_string(), "title".to_string()),
            true
        );
    }
}
