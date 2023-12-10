pub struct Solution;

impl Solution {
    pub fn decode_string(s: String) -> String {
        // We include any previous characters in the stack.
        let mut stack: Vec<(String, usize)> = Vec::new();
        let mut n = 0;
        let mut cur = String::new();
        for c in s.chars() {
            match c {
                '[' => {
                    stack.push((cur, n));
                    cur = String::new();
                    n = 0;
                }
                ']' => {
                    let (pre, x) = stack.pop().unwrap();
                    cur = pre + &cur.repeat(x);
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    n = n * 10 + c.to_digit(10).unwrap() as usize;
                }
                _ => cur.push(c),
            }
        }

        cur
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::decode_string("3[a]2[bc]".into()),
            "aaabcbc".to_string()
        );
        assert_eq!(
            Solution::decode_string("3[a2[c]]".into()),
            "accaccacc".to_string()
        );
        assert_eq!(
            Solution::decode_string("2[abc]3[cd]ef".into()),
            "abcabccdcdcdef".to_string()
        );
    }
}
