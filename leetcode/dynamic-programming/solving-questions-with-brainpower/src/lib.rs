pub struct Solution;

impl Solution {
    pub fn most_points(questions: Vec<Vec<i32>>) -> i64 {
        let mut dp = vec![0; questions.len()];
        Solution::helper(&questions, &mut dp, 0)
    }

    fn helper(questions: &[Vec<i32>], dp: &mut Vec<i64>, cur: usize) -> i64 {
        if cur >= questions.len() {
            return 0;
        } else if dp[cur] != 0 {
            return dp[cur];
        }

        let points = questions[cur][0] as i64;
        let skip = questions[cur][1] as usize;

        dp[cur] = (points + Solution::helper(questions, dp, cur + skip + 1)).max(Solution::helper(
            questions,
            dp,
            cur + 1,
        ));
        dp[cur]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::most_points(vec![vec![3, 2], vec![4, 3], vec![4, 4], vec![2, 5]]),
            5
        );

        assert_eq!(
            Solution::most_points(vec![
                vec![1, 1],
                vec![2, 2],
                vec![3, 3],
                vec![4, 4],
                vec![5, 5]
            ]),
            7
        );
    }
}
