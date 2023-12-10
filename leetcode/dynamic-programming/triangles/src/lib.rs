pub struct Solution;

impl Solution {
    pub fn minimum_total(mut triangle: Vec<Vec<i32>>) -> i32 {
        if triangle.len() == 1 {
            return triangle[0][0];
        }
        // We start at the bottom minus one row because the last row is already set.
        for i in (0..triangle.len() - 1).rev() {
            for j in 0..triangle[i].len() {
                triangle[i][j] = triangle[i][j] + triangle[i + 1][j].min(triangle[i + 1][j + 1]);
            }
        }
        triangle[0][0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let triangle = vec![vec![2], vec![3, 4], vec![6, 5, 7], vec![4, 1, 8, 3]];
        assert_eq!(Solution::minimum_total(triangle), 11);

        let triangle = vec![vec![-10]];
        assert_eq!(Solution::minimum_total(triangle), -10);
    }
}
