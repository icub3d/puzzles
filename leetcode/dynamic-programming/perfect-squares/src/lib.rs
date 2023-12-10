struct Solution;

impl Solution {
    pub fn num_squares(n: i32) -> i32 {
        let squares = (0..=n)
            .take_while(|&i| i * i <= n)
            .map(|i| i * i)
            .collect::<Vec<i32>>();
        let mut dp = vec![n + 1; n as usize + 1];
        dp[0] = 0;
        for i in 1..=n as usize {
            for &square in &squares {
                if i < square as usize {
                    break;
                }
                dp[i] = dp[i].min(dp[i - square as usize] + 1);
            }
        }
        dp[n as usize] as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::num_squares(12), 3);
        assert_eq!(Solution::num_squares(13), 2);
        assert_eq!(Solution::num_squares(1), 1);
    }
}
