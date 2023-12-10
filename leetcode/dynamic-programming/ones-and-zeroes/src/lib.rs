pub struct Solution;

impl Solution {
    pub fn find_max_form(strs: Vec<String>, m: i32, n: i32) -> i32 {
        let strs = strs
            .iter()
            .map(|s| {
                let (mut zeros, mut ones) = (0, 0);
                for c in s.chars() {
                    if c == '0' {
                        zeros += 1;
                    } else {
                        ones += 1;
                    }
                }
                (zeros, ones)
            })
            .collect::<Vec<_>>();

        let mut dp = vec![vec![0; n as usize + 1]; m as usize + 1];
        for s in strs {
            for i in (s.0..=m).rev() {
                for j in (s.1..=n).rev() {
                    dp[i as usize][j as usize] = dp[i as usize][j as usize]
                        .max(dp[(i - s.0) as usize][(j - s.1) as usize] + 1);
                }
            }
        }
        dp[m as usize][n as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::find_max_form(
                vec![
                    "10".to_string(),
                    "0001".to_string(),
                    "111001".to_string(),
                    "1".to_string(),
                    "0".to_string()
                ],
                5,
                3
            ),
            4
        );
        assert_eq!(
            Solution::find_max_form(
                vec!["10".to_string(), "0".to_string(), "1".to_string()],
                1,
                1
            ),
            2
        );
    }
}
