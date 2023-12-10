pub struct Solution;

use std::collections::HashMap;
impl Solution {
    pub fn min_cost_climbing_stairs(cost: Vec<i32>) -> i32 {
        let mut memo = HashMap::new();
        Solution::min_cost_climbing_stairs_helper(&cost, &mut memo)
    }

    pub fn min_cost_climbing_stairs_helper(cc: &[i32], memo: &mut HashMap<usize, i32>) -> i32 {
        match cc.len() {
            0 | 1 => 0,
            _ => {
                if let Some(v) = memo.get(&cc.len()) {
                    return *v;
                }
                let min = std::cmp::min(
                    cc[0] + Solution::min_cost_climbing_stairs_helper(&cc[1..], memo),
                    cc[1] + Solution::min_cost_climbing_stairs_helper(&cc[2..], memo),
                );
                memo.insert(cc.len(), min);
                min
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::min_cost_climbing_stairs(vec![10, 15]), 10);
        assert_eq!(Solution::min_cost_climbing_stairs(vec![15, 10]), 10);
        assert_eq!(Solution::min_cost_climbing_stairs(vec![10, 15, 20]), 15);
        assert_eq!(
            Solution::min_cost_climbing_stairs(vec![1, 100, 1, 1, 1, 100, 1, 1, 100, 1]),
            6
        );
    }
}
