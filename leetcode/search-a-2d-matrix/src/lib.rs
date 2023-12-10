pub struct Solution;

impl Solution {
    pub fn search_matrix(matrix: Vec<Vec<i32>>, target: i32) -> bool {
        // Just do a binary search but calculate l, m, r within matrix
        let n = matrix[0].len();
        let (mut l, mut r) = (0, matrix.len() * n - 1);
        if l == r {
            return matrix[0][0] == target;
        }

        while l < r {
            let m = (l + r) / 2;
            if matrix[m / n][m % n] >= target {
                r = m;
            } else {
                l = m + 1;
            }
        }
        matrix[l / n][l % n] == target
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let matrix = vec![vec![1, 3, 5, 7], vec![10, 11, 16, 20], vec![23, 30, 34, 60]];
        assert_eq!(Solution::search_matrix(matrix.clone(), 3), true);
        assert_eq!(Solution::search_matrix(matrix, 13), false);
        assert_eq!(Solution::search_matrix(vec![vec![1]], 0), false);
        assert_eq!(Solution::search_matrix(vec![vec![1]], 1), true);
    }
}
