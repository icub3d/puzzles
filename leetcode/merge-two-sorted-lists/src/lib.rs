// Definition for singly-linked list.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

impl ListNode {
    pub fn from(vals: &[i32]) -> Option<Box<ListNode>> {
        if vals.len() < 1 {
            None
        } else {
            Some(Box::new(ListNode {
                val: vals[0],
                next: Self::from(&vals[1..]),
            }))
        }
    }
}

pub fn merge_two_lists(
    list1: Option<Box<ListNode>>,
    list2: Option<Box<ListNode>>,
) -> Option<Box<ListNode>> {
    match (list1, list2) {
        (None, None) => None,
        (Some(l), None) => Some(l),
        (None, Some(r)) => Some(r),
        (Some(l), Some(r)) => match l.val <= r.val {
            true => Some(Box::new(ListNode {
                val: l.val,
                next: merge_two_lists(l.next, Some(r)),
            })),
            false => Some(Box::new(ListNode {
                val: r.val,
                next: merge_two_lists(Some(l), r.next),
            })),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::{merge_two_lists, ListNode};

    #[test]
    fn it_works() {
        assert_eq!(
            merge_two_lists(ListNode::from(&vec![]), ListNode::from(&vec![])),
            ListNode::from(&vec![])
        );
        assert_eq!(
            merge_two_lists(ListNode::from(&vec![]), ListNode::from(&vec![0])),
            ListNode::from(&vec![0])
        );
        assert_eq!(
            merge_two_lists(
                ListNode::from(&vec![1, 2, 4]),
                ListNode::from(&vec![1, 3, 4])
            ),
            ListNode::from(&vec![1, 1, 2, 3, 4, 4])
        );
    }
}
