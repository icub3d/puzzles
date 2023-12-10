pub struct Solution;

impl Solution {
    pub fn mincost_tickets(days: Vec<i32>, costs: Vec<i32>) -> i32 {
        let mut dp = vec![0; 366];
        let mut days_idx = 0;
        for i in 1..366 {
            if days_idx >= days.len() {
                break;
            }
            if i != days[days_idx] as usize {
                dp[i] = dp[i - 1];
            } else {
                dp[i] = std::cmp::min(
                    dp[i - 1] + costs[0],
                    std::cmp::min(
                        dp[std::cmp::max(0, i as i32 - 7) as usize] + costs[1],
                        dp[std::cmp::max(0, i as i32 - 30) as usize] + costs[2],
                    ),
                );
                days_idx += 1;
            }
        }
        dp[days[days.len() - 1] as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::mincost_tickets(vec![1, 4, 6, 7, 8, 20], vec![2, 7, 15]),
            11
        );
        assert_eq!(
            Solution::mincost_tickets(vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 31], vec![2, 7, 15]),
            17
        );
    }
}
