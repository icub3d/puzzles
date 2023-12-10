pub struct Solution;

impl Solution {
    pub fn max_profit_org(prices: Vec<i32>, fee: i32) -> i32 {
        // We want to track what happens for days we have stock and
        // days we don't.
        let n = prices.len();
        let mut held = vec![0; n];
        let mut sold = vec![0; n];

        // Initialize day 1.
        held[0] = -prices[0];

        for (i, price) in prices.iter().enumerate().skip(1) {
            // Max for holding would be to continue to hold or buying
            // today which means we were free the day before.
            held[i] = held[i - 1].max(sold[i - 1] - price);

            // Max for free would be to continue to be free or selling
            sold[i] = sold[i - 1].max(held[i - 1] + price - fee);
        }

        // Return the max of the last day being free or holding.
        held[n - 1].max(sold[n - 1])
    }

    pub fn max_profit(prices: Vec<i32>, fee: i32) -> i32 {
        // The intuition here is that we only use the previous prices,
        // so we just need to store the current value.
        let mut held = -prices[0];
        let mut sold = 0;

        for price in prices.iter().skip(1) {
            let held_tmp = held;
            held = held.max(sold - price);
            sold = sold.max(held_tmp + price - fee);
        }

        held.max(sold)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::max_profit(vec![1, 3, 2, 8, 4, 9], 2), 8);
        assert_eq!(Solution::max_profit(vec![1, 3, 7, 5, 10, 3], 3), 6);
    }
}
