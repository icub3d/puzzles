struct Solution;

impl Solution {
    pub fn min_cost_ii(costs: Vec<Vec<i32>>) -> i32 {
        let mut memo = vec![vec![0; costs[0].len()]; costs.len()];
        let mut min = i32::MAX;
        for i in 0..costs[0].len() {
            min = min.min(Self::dfs(&costs, &mut memo, 0, i));
        }
        min
    }
    fn dfs(costs: &Vec<Vec<i32>>, memo: &mut Vec<Vec<i32>>, i: usize, j: usize) -> i32 {
        if i == costs.len() {
            return 0;
        }
        if memo[i][j] != 0 {
            return memo[i][j];
        }
        let mut min = i32::MAX;
        for k in 0..costs[0].len() {
            if k != j {
                min = min.min(Self::dfs(costs, memo, i + 1, k));
            }
        }
        memo[i][j] = min + costs[i][j];
        memo[i][j]
    }
}

fn main() {
    assert_eq!(Solution::min_cost_ii(vec![vec![1, 5, 3], vec![2, 9, 4]]), 5);
    assert_eq!(Solution::min_cost_ii(vec![vec![1, 3]]), 5);
}
