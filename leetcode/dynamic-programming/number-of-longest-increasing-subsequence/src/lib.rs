pub struct Solution;

impl Solution {
    pub fn find_number_of_lis(nums: Vec<i32>) -> i32 {
        let mut dp = vec![1; nums.len()];
        let mut count = vec![1; nums.len()];

        for i in 0..nums.len() {
            for j in 0..i {
                if nums[i] > nums[j] {
                    if dp[j] + 1 > dp[i] {
                        dp[i] = dp[j] + 1;
                        count[i] = count[j];
                    } else if dp[j] + 1 == dp[i] {
                        count[i] += count[j];
                    }
                }
            }
        }

        let max = dp.iter().max().unwrap_or(&0);
        let mut ans = 0;
        for i in 0..dp.len() {
            if dp[i] == *max {
                ans += count[i];
            }
        }
        ans
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::find_number_of_lis(vec![1, 3, 5, 4, 7]), 2);
        assert_eq!(Solution::find_number_of_lis(vec![2, 2, 2, 2, 2]), 5);
    }
}
