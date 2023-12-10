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

pub fn middle_node(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
    let mut len = 0;
    let mut cur = head.clone();
    loop {
        match cur {
            None => break,
            Some(n) => {
                len += 1;
                cur = n.next
            }
        };
    }

    let mid = len / 2;
    let mut cur = head;
    for _ in 0..mid {
        match cur {
            None => break,
            Some(n) => {
                len += 1;
                cur = n.next
            }
        };
    }
    cur
}

#[cfg(test)]
mod tests {
    use super::{middle_node, ListNode};

    #[test]
    fn it_works() {
        assert_eq!(
            middle_node(ListNode::from(&vec![1, 2, 3, 4, 5])),
            ListNode::from(&vec![3, 4, 5])
        );
        assert_eq!(
            middle_node(ListNode::from(&vec![1, 2, 3, 4, 5, 6])),
            ListNode::from(&vec![4, 5, 6])
        );
    }
}
