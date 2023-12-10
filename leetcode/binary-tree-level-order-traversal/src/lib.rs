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
    pub fn level_order(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<Vec<i32>> {
        let mut result = vec![];
        Solution::helper(&mut result, root, 0);
        result
    }

    pub fn helper(vv: &mut Vec<Vec<i32>>, root: Option<Rc<RefCell<TreeNode>>>, depth: usize) {
        match root {
            Some(root) => {
                if depth + 1 > vv.len() {
                    vv.push(vec![]);
                }

                let root = root.borrow();
                vv[depth].push(root.val);
                Solution::helper(vv, root.left.clone(), depth + 1);
                Solution::helper(vv, root.right.clone(), depth + 1);
            }
            None => return,
        };
    }
}
