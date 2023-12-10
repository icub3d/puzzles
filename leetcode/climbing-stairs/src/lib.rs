pub struct Solution;

use std::collections::HashMap;
impl Solution {
    pub fn climb_stairs(n: i32) -> i32 {
        let mut memo = HashMap::new();
        memo.insert(1, 1);
        memo.insert(2, 2);
        memo.insert(3, 3);
        Solution::climb_stairs_helper(&mut memo, n)
    }

    pub fn climb_stairs_helper(memo: &mut HashMap<i32, i32>, n: i32) -> i32 {
        if let Some(n) = memo.get(&n) {
            return *n;
        }

        let i =
            Solution::climb_stairs_helper(memo, n - 1) + Solution::climb_stairs_helper(memo, n - 2);
        memo.insert(n, i);
        i
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
}
