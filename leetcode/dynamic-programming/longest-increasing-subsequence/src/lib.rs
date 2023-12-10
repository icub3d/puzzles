pub struct Solution;

impl Solution {
    pub fn length_of_lis(nums: Vec<i32>) -> i32 {
        // Base case for each is 1. That is we don't keep any but just
        // that element.
        let mut dp = vec![1; nums.len()];
        let mut max = 0;
        for i in 0..nums.len() {
            for j in 0..i {
                if nums[i] > nums[j] {
                    dp[i] = std::cmp::max(dp[i], dp[j] + 1);
                }
            }
            max = std::cmp::max(max, dp[i]);
        }
        max
    }

    pub fn length_of_lis_dfs(nums: Vec<i32>) -> i32 {
        let mut memo = vec![1; nums.len()];
        for i in (0..nums.len()).rev() {
            for j in i + 1..nums.len() {
                if nums[i] < nums[j] {
                    memo[i] = std::cmp::max(memo[i], memo[j] + 1);
                }
            }
        }
        *memo.iter().max().unwrap_or(&0)
    }

    pub fn length_of_lis_bsearch(nums: Vec<i32>) -> i32 {
        // The key here is to keep a subsequence that is always
        // sorted. If we find a number that wouldn't fit, instead of
        // skipping it, we replace it with the nearest value to it.
        let mut sub = vec![nums[0]];
        for i in 1..nums.len() {
            if nums[i] > sub[sub.len() - 1] {
                sub.push(nums[i]);
            } else {
                let idx = sub.binary_search(&nums[i]).unwrap_or_else(|x| x);
                sub[idx] = nums[i];
            }
        }
        sub.len() as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::length_of_lis(vec![10, 9, 2, 5, 3, 7, 101, 18]), 4);
        assert_eq!(Solution::length_of_lis(vec![0, 1, 0, 3, 2, 3]), 4);
        assert_eq!(Solution::length_of_lis(vec![7, 7, 7, 7, 7, 7, 7]), 1);
    }

    #[test]
    fn bsearch() {
        assert_eq!(
            Solution::length_of_lis_bsearch(vec![10, 9, 2, 5, 3, 7, 101, 18]),
            4
        );
        assert_eq!(Solution::length_of_lis_bsearch(vec![0, 1, 0, 3, 2, 3]), 4);
        assert_eq!(
            Solution::length_of_lis_bsearch(vec![7, 7, 7, 7, 7, 7, 7]),
            1
        );
    }

    #[test]
    fn dfs() {
        assert_eq!(
            Solution::length_of_lis_dfs(vec![10, 9, 2, 5, 3, 7, 101, 18]),
            4
        );
        assert_eq!(Solution::length_of_lis_dfs(vec![0, 1, 0, 3, 2, 3]), 4);
        assert_eq!(Solution::length_of_lis_dfs(vec![7, 7, 7, 7, 7, 7, 7]), 1);
    }
}
