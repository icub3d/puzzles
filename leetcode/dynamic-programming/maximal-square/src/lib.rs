pub struct Solution;

impl Solution {
    pub fn maximal_square(matrix: Vec<Vec<char>>) -> i32 {
        let mut max = 0;
        let mut dp = vec![vec![0; matrix[0].len() + 1]; matrix.len() + 1];
        for i in 1..=matrix.len() {
            for j in 1..=matrix[0].len() {
                if matrix[i - 1][j - 1] == '1' {
                    dp[i][j] =
                        std::cmp::min(std::cmp::min(dp[i - 1][j], dp[i][j - 1]), dp[i - 1][j - 1])
                            + 1;
                    max = std::cmp::max(max, dp[i][j]);
                }
            }
        }
        max * max
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let matrix = vec![
            vec!['1', '0', '1', '0', '0'],
            vec!['1', '0', '1', '1', '1'],
            vec!['1', '1', '1', '1', '1'],
            vec!['1', '0', '0', '1', '0'],
        ];
        assert_eq!(Solution::maximal_square(matrix), 4);

        let matrix = vec![vec!['0', '1'], vec!['1', '0']];
        assert_eq!(Solution::maximal_square(matrix), 1);

        let matrix = vec![vec!['0']];
        assert_eq!(Solution::maximal_square(matrix), 0);
    }
}
