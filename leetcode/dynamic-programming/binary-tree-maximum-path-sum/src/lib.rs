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
use std::rc::Rc;

pub struct Solution;

impl Solution {
    pub fn max_path_sum(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        let mut max = std::i32::MIN;
        Solution::max_path_sum_helper(root, &mut max);
        max
    }

    pub fn max_path_sum_helper(root: Option<Rc<RefCell<TreeNode>>>, max: &mut i32) -> i32 {
        match root {
            None => 0,
            Some(node) => {
                let left = Solution::max_path_sum_helper(node.borrow().left.clone(), max);
                let right = Solution::max_path_sum_helper(node.borrow().right.clone(), max);
                let val = node.borrow().val;
                *max = (*max).max(left + right + val);
                0.max(left.max(right) + val)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let root = Some(Rc::new(RefCell::new(TreeNode::new(1))));
        assert_eq!(Solution::max_path_sum(root), 1);

        let root = Some(Rc::new(RefCell::new(TreeNode::new(1))));
        root.as_ref().unwrap().borrow_mut().left = Some(Rc::new(RefCell::new(TreeNode::new(2))));
        root.as_ref().unwrap().borrow_mut().right = Some(Rc::new(RefCell::new(TreeNode::new(3))));
        assert_eq!(Solution::max_path_sum(root), 6);

        let root = Some(Rc::new(RefCell::new(TreeNode::new(-10))));
        root.as_ref().unwrap().borrow_mut().left = Some(Rc::new(RefCell::new(TreeNode::new(9))));
        root.as_ref().unwrap().borrow_mut().right = Some(Rc::new(RefCell::new(TreeNode::new(20))));
        root.as_ref()
            .unwrap()
            .borrow_mut()
            .right
            .as_ref()
            .unwrap()
            .borrow_mut()
            .left = Some(Rc::new(RefCell::new(TreeNode::new(15))));
        root.as_ref()
            .unwrap()
            .borrow_mut()
            .right
            .as_ref()
            .unwrap()
            .borrow_mut()
            .right = Some(Rc::new(RefCell::new(TreeNode::new(7))));
        assert_eq!(Solution::max_path_sum(root), 42);
    }
}
