pub struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn delete_and_earn(nums: Vec<i32>) -> i32 {
        // Track how much each number is worth. We'd take all of them if we
        // picked that number, so we need to know how much it's worth.
        let mut freq = HashMap::new();
        let mut max = 0;
        for num in nums {
            *freq.entry(num).or_insert(0) += num;
            max = max.max(num);
        }

        let mut memo = HashMap::new();
        Solution::delete_and_earn_helper(&mut freq, max, &mut memo)
    }

    pub fn delete_and_earn_helper(
        freq: &mut HashMap<i32, i32>,
        n: i32,
        memo: &mut HashMap<i32, i32>,
    ) -> i32 {
        if n == 0 {
            return 0;
        } else if n == 1 {
            return freq.get(&1).unwrap_or(&0).clone();
        } else if memo.contains_key(&n) {
            return memo.get(&n).unwrap().clone();
        }

        let max = Solution::delete_and_earn_helper(freq, n - 1, memo)
            .max(Solution::delete_and_earn_helper(freq, n - 2, memo) + freq.get(&n).unwrap_or(&0));
        memo.insert(n, max);
        max
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::delete_and_earn(vec![3, 4, 2]), 6);
        assert_eq!(Solution::delete_and_earn(vec![2, 2, 3, 3, 3, 4]), 9);
        assert_eq!(Solution::delete_and_earn(vec![3, 3, 3, 4, 2]), 9);
        assert_eq!(
            Solution::delete_and_earn(vec![8, 10, 4, 9, 1, 3, 5, 9, 4, 10]),
            37
        );
    }
}
