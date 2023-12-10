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
    pub fn preorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        match root {
            None => vec![],
            Some(root) => {
                let root = root.borrow();
                let mut result = vec![root.val];
                result.extend(Solution::preorder_traversal(root.left.clone()));
                result.extend(Solution::preorder_traversal(root.right.clone()));
                result
            }
        }
    }
}
