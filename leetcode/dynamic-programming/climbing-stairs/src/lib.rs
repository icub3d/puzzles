pub struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn climb_stairs_tab_space(n: i32) -> i32 {
        // We can improve space complexity with because we know we
        // only need the previous two values.
        if n <= 3 {
            return n;
        }

        let n = n as usize;
        let mut pp = 1;
        let mut p = 2;
        let mut cur = 3;

        for _ in 2..n {
            cur = pp + p;
            pp = p;
            p = cur;
        }
        cur
    }

    pub fn climb_stairs_tab(n: i32) -> i32 {
        // We can use tabulation, much like fibonacci. The next value
        // is the sum of the previous two.
        let n = n as usize;
        let mut dp = vec![0; n];
        dp[0] = 1;
        dp[1] = 2;
        for i in 2..n {
            dp[i] = dp[i - 1] + dp[i - 2];
        }
        dp[n - 1]
    }

    pub fn climb_stairs(n: i32) -> i32 {
        let mut m = HashMap::new();
        m.insert(1, 1);
        m.insert(2, 2);
        m.insert(3, 3);
        Solution::climb_stairs_helper(n, &mut m)
    }
    pub fn climb_stairs_helper(n: i32, m: &mut HashMap<i32, i32>) -> i32 {
        if m.contains_key(&n) {
            return *m.get(&n).unwrap();
        }
        let result =
            Solution::climb_stairs_helper(n - 1, m) + Solution::climb_stairs_helper(n - 2, m);
        m.insert(n, result);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::climb_stairs(2), 2);
        assert_eq!(Solution::climb_stairs(3), 3);
        assert_eq!(Solution::climb_stairs(10), 89);
    }

    #[test]
    fn climb_stairs_tab() {
        assert_eq!(Solution::climb_stairs_tab(2), 2);
        assert_eq!(Solution::climb_stairs_tab(3), 3);
        assert_eq!(Solution::climb_stairs_tab(10), 89);
    }

    #[test]
    fn climb_stairs_tab_space() {
        assert_eq!(Solution::climb_stairs_tab_space(2), 2);
        assert_eq!(Solution::climb_stairs_tab_space(3), 3);
        assert_eq!(Solution::climb_stairs_tab_space(10), 89);
    }
}
