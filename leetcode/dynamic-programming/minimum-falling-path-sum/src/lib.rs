pub struct Solution;

impl Solution {
    pub fn min_falling_path_sum(mut matrix: Vec<Vec<i32>>) -> i32 {
        if matrix.len() == 1 {
            return matrix[0][0];
        }
        for i in (0..matrix.len() - 1).rev() {
            for j in 0..matrix[i].len() {
                let mut min = matrix[i + 1][j];
                if j > 0 {
                    min = min.min(matrix[i + 1][j - 1]);
                }
                if j < matrix[i].len() - 1 {
                    min = min.min(matrix[i + 1][j + 1]);
                }
                matrix[i][j] += min;
            }
        }
        *matrix[0].iter().min().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let matrix = vec![vec![3]];
        assert_eq!(Solution::min_falling_path_sum(matrix), 3);

        let matrix = vec![vec![2, 1, 3], vec![6, 5, 4], vec![7, 8, 9]];
        assert_eq!(Solution::min_falling_path_sum(matrix), 13);

        let matrix = vec![vec![-19, 57], vec![-40, -5]];
        assert_eq!(Solution::min_falling_path_sum(matrix), -59);
    }
}
