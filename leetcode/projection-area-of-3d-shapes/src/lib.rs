pub struct Solution;

impl Solution {
    pub fn projection_area(grid: Vec<Vec<i32>>) -> i32 {
        let mut result = 0;
        let mut max_row = vec![0; grid.len()];
        let mut max_col = vec![0; grid[0].len()];
        for i in 0..grid.len() {
            for j in 0..grid[0].len() {
                if grid[i][j] > 0 {
                    result += 1;
                }
                max_row[i] = max_row[i].max(grid[i][j]);
                max_col[j] = max_col[j].max(grid[i][j]);
            }
        }
        for i in 0..grid.len() {
            result += max_row[i];
        }
        for j in 0..grid[0].len() {
            result += max_col[j];
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::projection_area(vec![vec![1, 2], vec![3, 4]]), 17);
        assert_eq!(Solution::projection_area(vec![vec![2]]), 5);
        assert_eq!(Solution::projection_area(vec![vec![1, 0], vec![0, 2]]), 8);
    }
}
