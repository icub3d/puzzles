pub struct Solution;

use std::collections::HashMap;
impl Solution {
    pub fn fib(n: i32) -> i32 {
        let mut memo = HashMap::new();
        memo.insert(0, 0);
        memo.insert(1, 1);
        Solution::fib_helper(&mut memo, n)
    }

    pub fn fib_helper(memo: &mut HashMap<i32, i32>, n: i32) -> i32 {
        if let Some(i) = memo.get(&n) {
            return *i;
        }

        let i = Solution::fib_helper(memo, n - 1) + Solution::fib_helper(memo, n - 2);
        memo.insert(n, i);
        i
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::fib(2), 1);
        assert_eq!(Solution::fib(3), 2);
        assert_eq!(Solution::fib(4), 3);
    }
}
