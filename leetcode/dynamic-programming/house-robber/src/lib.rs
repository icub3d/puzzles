pub struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn rob(nums: Vec<i32>) -> i32 {
        Self::rob_helper(&nums, &mut HashMap::new())
    }

    pub fn rob_helper(nums: &[i32], memo: &mut HashMap<usize, i32>) -> i32 {
        if nums.len() == 0 {
            return 0;
        } else if nums.len() == 1 {
            return nums[0];
        } else if memo.contains_key(&nums.len()) {
            return *memo.get(&nums.len()).unwrap();
        }

        let used = nums[0] + Self::rob_helper(&nums[2..], memo);
        let not_used = Self::rob_helper(&nums[1..], memo);
        let best = used.max(not_used);
        memo.insert(nums.len(), best);
        best
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::rob(vec![1, 2, 3, 1]), 4);
        assert_eq!(Solution::rob(vec![2, 7, 9, 3, 1]), 12);
    }
}
