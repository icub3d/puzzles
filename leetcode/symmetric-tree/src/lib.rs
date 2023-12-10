#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
    pub val: i32,
    pub left: Option<Rc<RefCell<TreeNode>>>,
    pub right: Option<Rc<RefCell<TreeNode>>>,
}

use std::cell::RefCell;
use std::rc::Rc;

pub struct Solution;

impl Solution {
    pub fn is_symmetric(root: Option<Rc<RefCell<TreeNode>>>) -> bool {
        match root {
            None => true,
            Some(root) => Solution::helper(root.borrow().left.clone(), root.borrow().right.clone()),
        }
    }

    pub fn helper(
        left: Option<Rc<RefCell<TreeNode>>>,
        right: Option<Rc<RefCell<TreeNode>>>,
    ) -> bool {
        match (left, right) {
            (None, None) => true,
            (Some(l), Some(r)) => {
                let left = l.borrow();
                let right = r.borrow();
                if left.val != right.val {
                    false
                } else {
                    Solution::helper(left.right.clone(), right.left.clone())
                        && Solution::helper(left.left.clone(), right.right.clone())
                }
            }
            _ => false,
        }
    }
}
