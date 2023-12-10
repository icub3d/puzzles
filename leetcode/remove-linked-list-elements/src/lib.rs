// Definition for singly-linked list.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

impl ListNode {
    #[inline]
    fn new(val: i32) -> Self {
        ListNode { next: None, val }
    }
    pub fn from(v: &[i32]) -> Option<Box<ListNode>> {
        if v.len() == 0 {
            return None;
        }

        return Some(Box::new(ListNode {
            val: v[0],
            next: ListNode::from(&v[1..]),
        }));
    }
}

pub struct Solution;

impl Solution {
    pub fn remove_elements(mut head: Option<Box<ListNode>>, val: i32) -> Option<Box<ListNode>> {
        let mut h = &mut head;

        loop {
            match h {
                Some(n) if n.val == val => *h = n.next.take(),
                Some(n) => h = &mut n.next,
                None => break,
            }
        }
        head
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::remove_elements(ListNode::from(&vec![1, 2, 6, 3, 4, 5, 6]), 6),
            ListNode::from(&vec![1, 2, 3, 4, 5])
        );
        assert_eq!(
            Solution::remove_elements(ListNode::from(&vec![7, 7, 7, 7]), 7),
            ListNode::from(&vec![])
        );
    }
}
