pub struct Solution;

impl Solution {
    pub fn longest_arith_seq_length(nums: Vec<i32>) -> i32 {
        use std::collections::HashMap;
        let mut dp: HashMap<(i32, i32), i32> = HashMap::new();
        let mut solution = 0;
        for i in 0..nums.len() {
            for j in 0..i {
                let diff = nums[i] - nums[j];
                let count = dp.get(&(j as i32, diff)).unwrap_or(&1) + 1;
                dp.insert((i as i32, diff), count);
                solution = solution.max(count);
            }
        }
        solution
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::longest_arith_seq_length(vec![1, 2, 3, 4]), 4);
        assert_eq!(Solution::longest_arith_seq_length(vec![1, 3, 5, 7]), 4);
        assert_eq!(
            Solution::longest_arith_seq_length(vec![1, 5, 7, 8, 5, 3, 4, 2, 1]),
            4
        );
        assert_eq!(Solution::longest_arith_seq_length(vec![9, 4, 7, 2, 10]), 3);
        assert_eq!(
            Solution::longest_arith_seq_length(vec![20, 1, 15, 3, 10, 5, 8]),
            4
        );
    }
}
