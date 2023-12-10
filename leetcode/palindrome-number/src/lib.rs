pub struct Solution;

impl Solution {
    pub fn is_palindrome(x: i32) -> bool {
        if x < 0 {
            return false;
        } else if x < 10 {
            return true;
        }

        let nn = {
            let mut v = vec![];
            let mut x = x;
            while x > 0 {
                v.push(x % 10);
                x /= 10;
            }
            v
        };

        let mut l = 0;
        let mut r = nn.len() - 1;
        while l < r {
            if nn[l] != nn[r] {
                return false;
            }
            l += 1;
            r -= 1;
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::is_palindrome(121), true);
        assert_eq!(Solution::is_palindrome(-121), false);
        assert_eq!(Solution::is_palindrome(10), false);
        assert_eq!(Solution::is_palindrome(-101), false);
    }
}
