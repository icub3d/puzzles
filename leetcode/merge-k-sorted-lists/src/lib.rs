pub struct Solution;

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

    fn from(vec: Vec<i32>) -> Option<Box<ListNode>> {
        let mut head = None;
        for &val in vec.iter().rev() {
            let mut node = ListNode::new(val);
            node.next = head;
            head = Some(Box::new(node));
        }
        head
    }
}

use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;

impl Solution {
    pub fn merge_k_lists(lists: Vec<Option<Box<ListNode>>>) -> Option<Box<ListNode>> {
        if lists.is_empty() {
            return None;
        }

        let mut head = None;
        let mut tail = &mut head;

        // Use a heap to track which one will be next.
        let mut heap = lists
            .into_iter()
            .filter(Option::is_some) // only include non-empty lists
            .map(Reverse) // smallest first
            .collect::<BinaryHeap<_>>();

        while let Some(Reverse(mut node)) = heap.pop() {
            let next = node.as_mut().and_then(|n| n.next.take());
            if next.is_some() {
                heap.push(Reverse(next));
            }
            *tail = node;
            tail = &mut tail.as_mut().unwrap().next;
        }

        head
    }
}

impl PartialOrd for ListNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ListNode {
    fn cmp(&self, other: &Self) -> Ordering {
        self.val.cmp(&other.val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::merge_k_lists(vec![]), None);
        assert_eq!(
            Solution::merge_k_lists(vec![ListNode::from(vec![1, 2, 3, 4])]),
            ListNode::from(vec![1, 2, 3, 4])
        );
        assert_eq!(
            Solution::merge_k_lists(vec![
                ListNode::from(vec![1, 4, 5]),
                ListNode::from(vec![1, 3, 4]),
                ListNode::from(vec![2, 6]),
            ]),
            ListNode::from(vec![1, 1, 2, 3, 4, 4, 5, 6])
        );
    }
}
