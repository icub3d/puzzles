pub struct Solution;

use std::collections::HashMap;
impl Solution {
    pub fn unique_paths(m: i32, n: i32) -> i32 {
        let mut memo = HashMap::new();
        memo.insert((1, 1), 1);
        memo.insert((1, 2), 1);
        memo.insert((2, 1), 1);
        memo.insert((2, 2), 2);
        Solution::helper(m, n, &mut memo)
    }

    pub fn helper(m: i32, n: i32, memo: &mut HashMap<(i32, i32), i32>) -> i32 {
        if m < 1 || n < 1 {
            return 0;
        }
        if let Some(v) = memo.get(&(m, n)) {
            return *v;
        }
        let s = Solution::helper(m - 1, n, memo) + Solution::helper(m, n - 1, memo);
        memo.insert((m, n), s);
        memo.insert((n, m), s);
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::unique_paths(3, 7), 28);
        assert_eq!(Solution::unique_paths(3, 2), 3);
    }
}
