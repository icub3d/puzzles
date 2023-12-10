pub struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn coin_change(coins: Vec<i32>, amount: i32) -> i32 {
        let mut memo = HashMap::new();
        Solution::helper(&mut memo, &coins, amount)
    }

    pub fn helper(memo: &mut HashMap<i32, i32>, counts: &[i32], remaining: i32) -> i32 {
        if remaining == 0 {
            return 0;
        }
        if remaining < 0 {
            return -1;
        }
        if let Some(&count) = memo.get(&remaining) {
            return count;
        }
        let mut min = std::i32::MAX;
        for &coin in counts {
            let count = Solution::helper(memo, counts, remaining - coin);
            if count >= 0 && count < min {
                min = count + 1;
            }
        }
        let count = if min == std::i32::MAX { -1 } else { min };
        memo.insert(remaining, count);
        count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::coin_change(vec![1, 2, 5], 11), 3);
        assert_eq!(Solution::coin_change(vec![2], 3), -1);
        assert_eq!(Solution::coin_change(vec![1], 0), 0);
    }
}
