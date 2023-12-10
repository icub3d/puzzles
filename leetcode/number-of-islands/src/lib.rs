pub struct Solution;

use std::collections::HashSet;
impl Solution {
    pub fn num_islands(grid: Vec<Vec<char>>) -> i32 {
        let mut visited: HashSet<(usize, usize)> = HashSet::new();
        let mut islands = 0;
        let height = grid.len();
        let width = grid[0].len();

        for m in 0..height {
            for n in 0..width {
                if grid[m][n] == '1' && !visited.contains(&(m, n)) {
                    islands += 1;
                    Solution::fill(&grid, &mut visited, m, n, height, width);
                }
            }
        }

        islands
    }

    pub fn fill(
        grid: &Vec<Vec<char>>,
        visited: &mut HashSet<(usize, usize)>,
        m: usize,
        n: usize,
        height: usize,
        width: usize,
    ) {
        visited.insert((m, n));
        if m > 0 && grid[m - 1][n] == '1' && !visited.contains(&(m - 1, n)) {
            Solution::fill(grid, visited, m - 1, n, height, width);
        }
        if n > 0 && grid[m][n - 1] == '1' && !visited.contains(&(m, n - 1)) {
            Solution::fill(grid, visited, m, n - 1, height, width);
        }
        if m < height - 1 && grid[m + 1][n] == '1' && !visited.contains(&(m + 1, n)) {
            Solution::fill(grid, visited, m + 1, n, height, width);
        }
        if n < width - 1 && grid[m][n + 1] == '1' && !visited.contains(&(m, n + 1)) {
            Solution::fill(grid, visited, m, n + 1, height, width);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::num_islands(vec![
                vec!['1', '1', '1', '1', '0'],
                vec!['1', '1', '0', '1', '0'],
                vec!['1', '1', '0', '0', '0'],
                vec!['0', '0', '0', '0', '0']
            ]),
            1
        );
        assert_eq!(
            Solution::num_islands(vec![
                vec!['1', '1', '0', '0', '0'],
                vec!['1', '1', '0', '0', '0'],
                vec!['0', '0', '1', '0', '0'],
                vec!['0', '0', '0', '1', '1']
            ]),
            3
        );
    }
}
