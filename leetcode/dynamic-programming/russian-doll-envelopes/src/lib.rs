pub struct Solution;

impl Solution {
    pub fn max_envelopes(envelopes: Vec<Vec<i32>>) -> i32 {
        let mut ee = envelopes.clone();
        ee.sort_by(|a, b| a[0].cmp(&b[0]).then(b[1].cmp(&a[1])));

        let mut dp = vec![];
        for e in &ee {
            let i = dp.binary_search(&e[1]).unwrap_or_else(|x| x);
            if i >= dp.len() {
                dp.push(e[1]);
            } else {
                dp[i] = e[1];
            }
        }
        dp.len() as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::max_envelopes(vec![vec![5, 4], vec![6, 4], vec![6, 7], vec![2, 3]]),
            3
        );
        assert_eq!(
            Solution::max_envelopes(vec![vec![1, 1], vec![1, 1], vec![1, 1]]),
            1
        );
    }
}
