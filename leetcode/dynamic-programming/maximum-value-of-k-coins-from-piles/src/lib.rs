pub struct Solution;

impl Solution {
    pub fn max_value_of_coins(piles: Vec<Vec<i32>>, k: i32) -> i32 {
        let k = k as usize;
        let mut dp = vec![0; k + 1];

        for pile in &piles {
            let mut cur = vec![0; k + 1];
            for coin in 0..=k {
                let mut total = 0;
                for picket in 0..=pile.len().min(coin) {
                    if picket > 0 {
                        total += pile[picket - 1];
                    }
                    cur[coin] = cur[coin].max(dp[coin - picket] + total);
                }
            }
            dp = cur;
        }
        dp[k]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::max_value_of_coins(vec![vec![1, 100, 3], vec![7, 8, 9]], 2),
            101
        );
        assert_eq!(
            Solution::max_value_of_coins(
                vec![
                    vec![100],
                    vec![100],
                    vec![100],
                    vec![100],
                    vec![100],
                    vec![100],
                    vec![1, 1, 1, 1, 1, 1, 700]
                ],
                7
            ),
            706
        );
    }
}
