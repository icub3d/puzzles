use std::collections::HashMap;

pub struct Solution;

impl Solution {
    pub fn min_cost_climbing_stairs(cost: Vec<i32>) -> i32 {
        Self::min_cost_climbing_stairs_helper(&cost, &mut HashMap::new())
    }

    pub fn min_cost_climbing_stairs_helper(cost: &[i32], memo: &mut HashMap<usize, i32>) -> i32 {
        if cost.len() <= 1 {
            return 0;
        }
        if let Some(&result) = memo.get(&cost.len()) {
            return result;
        }
        let result = std::cmp::min(
            cost[0] + Self::min_cost_climbing_stairs_helper(&cost[1..], memo),
            cost[1] + Self::min_cost_climbing_stairs_helper(&cost[2..], memo),
        );
        memo.insert(cost.len(), result);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::min_cost_climbing_stairs(vec![10, 15, 20]), 15);
        assert_eq!(
            Solution::min_cost_climbing_stairs(vec![1, 100, 1, 1, 1, 100, 1, 1, 100, 1]),
            6
        );
    }
}
