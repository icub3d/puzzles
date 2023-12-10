pub struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn combination_sum4(nums: Vec<i32>, target: i32) -> i32 {
        // Solution::bottom_up(&nums, target)
        let mut memo = HashMap::new();
        Solution::helper(&mut memo, &nums, target)
    }

    pub fn helper(memo: &mut HashMap<i32, i32>, nums: &[i32], target: i32) -> i32 {
        let mut count = 0;

        if memo.contains_key(&target) {
            return memo[&target];
        } else if target == 0 {
            return 1;
        }
        for &num in nums {
            if target >= num {
                count += Solution::helper(memo, nums, target - num);
            }
        }
        memo.insert(target, count);
        count
    }

    pub fn bottom_up(nums: &[i32], target: i32) -> i32 {
        let mut dp = vec![0_usize; target as usize + 1];
        dp[0] = 1;
        for i in 1..=target {
            for &num in nums {
                // Note the actual solution didn't work because of
                // overflow. Since the answer specifies if will fit in
                // a 32 bit signed integer, we can use that to our
                // advantage and ignore any values that overflow.
                if i >= num && dp[(i - num) as usize] <= (1_usize << 32) {
                    dp[i as usize] += dp[(i - num) as usize];
                }
            }
        }
        dp[target as usize] as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::combination_sum4(vec![1, 2, 3], 4), 7);
        assert_eq!(Solution::combination_sum4(vec![9], 3), 0);
        assert_eq!(Solution::combination_sum4(vec![4, 2, 1], 32), 39882198);
        assert_eq!(
            Solution::combination_sum4(
                vec![
                    10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170,
                    180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330,
                    340, 350, 360, 370, 380, 390, 400, 410, 420, 430, 440, 450, 460, 470, 480, 490,
                    500, 510, 520, 530, 540, 550, 560, 570, 580, 590, 600, 610, 620, 630, 640, 650,
                    660, 670, 680, 690, 700, 710, 720, 730, 740, 750, 760, 770, 780, 790, 800, 810,
                    820, 830, 840, 850, 860, 870, 880, 890, 900, 910, 920, 930, 940, 950, 960, 970,
                    980, 990, 111
                ],
                999
            ),
            1
        );
    }
}
