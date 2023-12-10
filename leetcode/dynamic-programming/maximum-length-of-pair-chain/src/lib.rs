pub struct Solution;

impl Solution {
    pub fn find_longest_chain(pairs: Vec<Vec<i32>>) -> i32 {
        let mut pairs = pairs.clone();
        pairs.sort();

        let mut dp = vec![1; pairs.len()];
        for i in 0..pairs.len() {
            for j in 0..i {
                // If the first element of the current pair is greater
                // than the second element of the previous pair, then
                // we can add the current pair to the chain.
                if pairs[i][0] > pairs[j][1] {
                    dp[i] = dp[i].max(dp[j] + 1);
                }
            }
        }
        *dp.iter().max().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::find_longest_chain(vec![vec![1, 2], vec![2, 3], vec![3, 4]]),
            2
        );
        assert_eq!(
            Solution::find_longest_chain(vec![vec![1, 2], vec![7, 8], vec![4, 5]]),
            3
        );
    }
}
