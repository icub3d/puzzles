pub struct Solution;

impl Solution {
    pub fn my_atoi(s: String) -> i32 {
        let mut chars = s.chars().peekable();
        while chars.peek() == Some(&' ') {
            chars.next();
        }

        let mut negative = false;
        if let Some(&'-') = chars.peek() {
            negative = true;
            chars.next();
        } else if let Some(&'+') = chars.peek() {
            chars.next();
        }

        let mut result = 0_i32;

        while let Some(&c) = chars.peek() {
            if c.is_digit(10) {
                let digit = c.to_digit(10).unwrap() as i32;
                if result > (i32::MAX - digit) / 10 {
                    return if negative { i32::MIN } else { i32::MAX };
                }
                result = result * 10 + digit;
            } else {
                break;
            }
            chars.next();
        }

        if negative {
            result = -result;
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::my_atoi("42".to_string()), 42);
        assert_eq!(Solution::my_atoi("   -42".to_string()), -42);
        assert_eq!(Solution::my_atoi("4193 with words".to_string()), 4193);
    }
}
