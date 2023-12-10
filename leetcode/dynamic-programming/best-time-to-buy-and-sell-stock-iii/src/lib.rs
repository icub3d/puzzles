#[derive(PartialEq, Eq)]
pub enum State {
    Empty,
    Hold(i32),
}

pub struct Solution;

impl Solution {
    pub fn max_profit_state(prices: Vec<i32>) -> i32 {
        // The intiution here is that we only need to track the state
        // of 4 positions. Buy and sell 1, buy and sell 2. We track
        // the cost and profits that would occur in each of those
        // places.
        let (mut t1_cost, mut t2_cost, mut t1_profit, mut t2_profit) =
            (std::i32::MAX, std::i32::MAX, 0, 0);

        for price in prices {
            // If this were the first transaction, determine the cost
            // and profit at this point.
            t1_cost = t1_cost.min(price);
            t1_profit = t1_profit.max(price - t1_cost);

            // If this were the second, determin the cost and profit
            // including the first transaction.
            t2_cost = t2_cost.min(price - t1_profit);
            t2_profit = t2_profit.max(price - t2_cost);
        }

        t2_profit
    }

    pub fn max_profit_recursive(prices: Vec<i32>) -> i32 {
        Solution::max_profit_helper(&prices, State::Empty, 0, 0)
    }

    pub fn max_profit_helper(prices: &[i32], state: State, bought: usize, profit: i32) -> i32 {
        if (bought == 2 && state == State::Empty) || prices.is_empty() {
            return profit;
        }

        match state {
            State::Empty => {
                // We can either buy or not buy.
                let buy = Solution::max_profit_helper(
                    &prices[1..],
                    State::Hold(prices[0]),
                    bought + 1,
                    profit,
                );
                let not_buy =
                    Solution::max_profit_helper(&prices[1..], State::Empty, bought, profit);
                std::cmp::max(buy, not_buy)
            }
            State::Hold(bought_price) => {
                // We can either sell or not sell.
                let sell = Solution::max_profit_helper(
                    &prices[1..],
                    State::Empty,
                    bought,
                    profit + prices[0] - bought_price,
                );
                let not_sell = Solution::max_profit_helper(
                    &prices[1..],
                    State::Hold(bought_price),
                    bought,
                    profit,
                );
                std::cmp::max(sell, not_sell)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test {
		( $($func:ident),* ) => {
			$(
				#[test]
				fn $func() {
					assert_eq!(Solution::$func(vec![3, 4, 5, 0, 0, 3, 1, 4]), 6);
					assert_eq!(Solution::$func(vec![1, 2, 3, 4, 5]), 4);
					assert_eq!(Solution::$func(vec![7, 6, 4, 3, 1]), 0);
				}
			)*
		};
	}

    test! { max_profit_recursive, max_profit_state }
}
