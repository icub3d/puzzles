pub struct Solution;

impl Solution {
    pub fn shifting_letters(s: String, shifts: Vec<i32>) -> String {
        let mut shifts = shifts;
        let mut sum = 0;
        for i in (0..shifts.len()).rev() {
            sum = (sum + shifts[i]) % 26;
            shifts[i] = sum;
        }
        let mut res = String::new();
        for (i, c) in s.chars().enumerate() {
            let c = c as u8 - b'a';
            let c = (c + shifts[i] as u8) % 26;
            res.push((c + b'a') as char);
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::shifting_letters("abc".to_string(), vec![3, 5, 9]),
            "rpl".to_string()
        );
        assert_eq!(
            Solution::shifting_letters("aaa".to_string(), vec![1, 2, 3]),
            "gfd".to_string()
        );
    }
}
