pub struct Solution;

impl Solution {
    pub fn max_profit(prices: Vec<i32>) -> i32 {
        let mut left = 0;
        let mut max: i32 = 0;
        for right in 1..prices.len() {
            // If we have a good price difference, let's check for max. We continue
            // because we can keep looking.
            if prices[right] > prices[left] {
                max = max.max(prices[right] - prices[left]);
                continue;
            }

            // We want to move the left window to the next reasonable position if we get here
            // because we won't have positive prices.
            while prices[left] > prices[right] && left < right {
                left += 1;
            }
        }
        max
    }
}

#[cfg(test)]
mod tests {
    use super::Solution;

    #[test]
    fn it_works() {
        assert_eq!(Solution::max_profit(vec![7, 1, 5, 3, 6, 4]), 5);
        assert_eq!(Solution::max_profit(vec![7, 6, 4, 3, 1]), 0);
    }
}
