pub struct Solution;

use std::collections::HashMap;

impl Solution {
    pub fn unique_paths_with_obstacles(grid: Vec<Vec<i32>>) -> i32 {
        let mut memo = HashMap::new();
        memo.insert((grid.len() - 1, grid[0].len() - 1), 1);
        Solution::unique_paths_with_obstacles_helper(&grid, 0, 0, &mut memo)
    }

    pub fn unique_paths_with_obstacles_helper(
        grid: &Vec<Vec<i32>>,
        row: usize,
        col: usize,
        memo: &mut HashMap<(usize, usize), i32>,
    ) -> i32 {
        if row >= grid.len() || col >= grid[0].len() {
            return 0;
        } else if grid[row][col] == 1 {
            return 0;
        } else if let Some(&val) = memo.get(&(row, col)) {
            return val;
        }

        let sum = Solution::unique_paths_with_obstacles_helper(grid, row + 1, col, memo)
            + Solution::unique_paths_with_obstacles_helper(grid, row, col + 1, memo);

        memo.insert((row, col), sum);
        sum
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::unique_paths_with_obstacles(vec![
                vec![0, 0, 0],
                vec![0, 1, 0],
                vec![0, 0, 0]
            ]),
            2
        );
        assert_eq!(
            Solution::unique_paths_with_obstacles(vec![vec![0, 1], vec![0, 0]]),
            1
        );
    }
}
