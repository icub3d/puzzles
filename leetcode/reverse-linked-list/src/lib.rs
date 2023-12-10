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

pub fn reverse_list(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
    let mut cur = head;
    let mut list = None;
    loop {
        match cur {
            None => break,
            Some(n) => {
                list = Some(Box::new(ListNode {
                    val: n.val,
                    next: list,
                }));
                cur = n.next;
            }
        }
    }

    list
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            reverse_list(ListNode::from(&vec![])),
            ListNode::from(&vec![])
        );
        assert_eq!(
            reverse_list(ListNode::from(&vec![1, 2, 3, 4, 5])),
            ListNode::from(&vec![5, 4, 3, 2, 1])
        );
    }
}
