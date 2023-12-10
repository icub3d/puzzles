pub struct Solution;

impl Solution {
    pub fn reverse(x: i32) -> i32 {
        let mut x = x;
        let mut result: i32 = 0;
        while x != 0 {
            match result.checked_mul(10).and_then(|r| r.checked_add(x % 10)) {
                Some(r) => result = r,
                None => return 0,
            }
            x /= 10;
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::reverse(-123), -321);
        assert_eq!(Solution::reverse(123), 321);
        assert_eq!(Solution::reverse(-123), -321);
        assert_eq!(Solution::reverse(120), 21);
        assert_eq!(Solution::reverse(0), 0);
        assert_eq!(Solution::reverse(1534236469), 0);
    }
}
