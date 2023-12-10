pub struct Solution;

impl Solution {
    pub fn min_path_sum(mut grid: Vec<Vec<i32>>) -> i32 {
        for col in (0..grid.len()).rev() {
            for row in (0..grid[0].len()).rev() {
                if col == grid.len() - 1 && row == grid[0].len() - 1 {
                    continue;
                } else if col == grid.len() - 1 {
                    grid[col][row] += grid[col][row + 1];
                } else if row == grid[0].len() - 1 {
                    grid[col][row] += grid[col + 1][row];
                } else {
                    grid[col][row] += std::cmp::min(grid[col + 1][row], grid[col][row + 1]);
                }
            }
        }
        grid[0][0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::min_path_sum(vec![vec![1, 2, 3], vec![4, 5, 6]]),
            12
        );
        assert_eq!(
            Solution::min_path_sum(vec![vec![1, 3, 1], vec![1, 5, 1], vec![4, 2, 1]]),
            7
        );
    }
}
