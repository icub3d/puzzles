use std::collections::HashMap;

pub struct Solution;

impl Solution {
    // We can improve space complexity with fib_tab by only tracking
    // the previous two values. We can do this because fib only needs
    // the previous two responses.
    pub fn fib_tab_space(n: i32) -> i32 {
        let n = n as usize;
        let mut pp = 0;
        let mut p = 1;
        let mut cur = pp + p;
        for _ in 2..=n {
            cur = pp + p;
            pp = p;
            p = cur;
        }
        cur
    }

    // We can use tabulation instead of recursion. We track results in
    // a table (equivalent to memoization) and then as we loop, we can
    // juse previous values.
    pub fn fib_tab(n: i32) -> i32 {
        let n = n as usize;
        let mut memo = vec![0; n + 1];
        memo[0] = 0;
        memo[1] = 1;
        for i in 2..=n {
            memo[i] = memo[i - 1] + memo[i - 2];
        }
        memo[n]
    }

    pub fn fib(n: i32) -> i32 {
        let mut memo = HashMap::new();
        memo.insert(0, 0);
        memo.insert(1, 1);
        Self::fib_helper(n, &mut memo)
    }

    pub fn fib_helper(n: i32, memo: &mut HashMap<i32, i32>) -> i32 {
        if memo.contains_key(&n) {
            return memo[&n];
        }
        let result = Self::fib_helper(n - 1, memo) + Self::fib_helper(n - 2, memo);
        memo.insert(n, result);
        result
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
        assert_eq!(Solution::fib(10), 55);
    }

    #[test]
    fn fib_tab() {
        assert_eq!(Solution::fib_tab(2), 1);
        assert_eq!(Solution::fib_tab(3), 2);
        assert_eq!(Solution::fib_tab(4), 3);
        assert_eq!(Solution::fib_tab(10), 55);
    }
    #[test]
    fn fib_tab_space() {
        assert_eq!(Solution::fib_tab_space(2), 1);
        assert_eq!(Solution::fib_tab_space(3), 2);
        assert_eq!(Solution::fib_tab_space(4), 3);
        assert_eq!(Solution::fib_tab_space(10), 55);
    }
}
