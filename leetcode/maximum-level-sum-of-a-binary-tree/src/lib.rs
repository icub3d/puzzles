// Definition for a binary tree node.
#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
    pub val: i32,
    pub left: Option<Rc<RefCell<TreeNode>>>,
    pub right: Option<Rc<RefCell<TreeNode>>>,
}

impl TreeNode {
    #[inline]
    pub fn new(val: i32) -> Self {
        TreeNode {
            val,
            left: None,
            right: None,
        }
    }
}

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

impl Solution {
    pub fn max_level_sum(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        let mut level = 1;
        let mut counts = HashMap::new();

        // Do a dfs and keep track of the level and the sum at that level.
        Solution::dfs(root, level, &mut counts);

        // Find the min level with the max sum.
        let mut max = i32::MIN;
        let mut level = 0;
        for (k, v) in counts {
            if v > max {
                max = v;
                level = k;
            }
        }
        level
    }

    pub fn dfs(root: Option<Rc<RefCell<TreeNode>>>, level: i32, counts: &mut HashMap<i32, i32>) {
        if let Some(node) = root {
            let mut node = node.borrow_mut();
            *counts.entry(level).or_insert(0) += node.val;
            Solution::helper(node.left.take(), level + 1, counts);
            Solution::helper(node.right.take(), level + 1, counts);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
