pub struct Solution;

impl Solution {
    pub fn longest_subsequence(arr: Vec<i32>, difference: i32) -> i32 {
        let mut dp: std::collections::HashMap<i32, i32> = std::collections::HashMap::new();
        let mut solution = 1;
        for a in arr {
            dp.insert(a, dp.get(&(a - difference)).unwrap_or(&0) + 1);
            solution = solution.max(dp[&a]);
        }
        solution
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::longest_subsequence(vec![1, 2, 3, 4], 1), 4);
        assert_eq!(Solution::longest_subsequence(vec![1, 3, 5, 7], 1), 1);
        assert_eq!(
            Solution::longest_subsequence(vec![1, 5, 7, 8, 5, 3, 4, 2, 1], -2),
            4
        );
    }
}
