pub struct Solution;

impl Solution {
    pub fn max_profit(prices: Vec<i32>) -> i32 {
        let mut sold = std::i32::MIN;
        let mut held = std::i32::MIN;
        let mut reset = 0;

        // At each step, we can calculate the maximum profit we'd get
        // from each action and keep track of them all. At the end,
        // the best result is in one of the three.
        for price in prices {
            let pre_sold = sold;
            // We would only sell if previous was held, so if we sold
            // at this point, it would be our current price + the
            // profit we'd get from the previous held.
            sold = held + price;

            // If we held, our hold price would be the max of
            // previously holding or reseting minus the current
            // price. The idea here is that if our best solution is to
            // hold at this point, it would be because this price
            // doesn't hurt us.
            held = std::cmp::max(held, reset - price);

            // If we reset, we'd just take the best of the previous
            // either having just sold or still resting.
            reset = std::cmp::max(reset, pre_sold);
        }
        sold.max(reset)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::max_profit(vec![1, 2, 3, 0, 2]), 3);
        assert_eq!(Solution::max_profit(vec![1]), 0);
    }
}
