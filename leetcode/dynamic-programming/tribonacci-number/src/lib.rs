use std::collections::HashMap;

pub struct Solution;

impl Solution {
    pub fn tribonacci(n: i32) -> i32 {
        let mut memo: HashMap<i32, i32> = HashMap::new();
        memo.insert(0, 0);
        memo.insert(1, 1);
        memo.insert(2, 1);
        Self::tribonacci_helper(n, &mut memo)
    }

    pub fn tribonacci_helper(n: i32, memo: &mut HashMap<i32, i32>) -> i32 {
        if memo.contains_key(&n) {
            return *memo.get(&n).unwrap();
        }
        let result = Self::tribonacci_helper(n - 1, memo)
            + Self::tribonacci_helper(n - 2, memo)
            + Self::tribonacci_helper(n - 3, memo);
        memo.insert(n, result);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::tribonacci(4), 4);
        assert_eq!(Solution::tribonacci(25), 1389537);
        assert_eq!(Solution::tribonacci(37), 2082876103);
    }
}
