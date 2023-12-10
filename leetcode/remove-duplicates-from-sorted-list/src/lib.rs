pub struct Solution;

pub struct List {
    head: ListNode,
    tail: Option<Box<ListNode>>,
}

impl List {
    pub fn new() -> Self {
        Self {
            head: ListNode { val: 0, next: None },
            tail: None,
        }
    }

    pub fn push(&mut self, val: i32) {
        let node = Some(Box::new(ListNode { val, next: None }));
        if self.head.next == None {
            self.head.next = node;
            self.tail = self.head.next;
        } else {
            self.tail.unwrap().next = node;
        }
        self.tail = node;
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

impl Solution {
    pub fn delete_duplicates(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        head
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn from(v: &[i32]) -> Option<Box<ListNode>> {
        if v.len() == 0 {
            return None;
        }
        Some(Box::new(ListNode {
            val: v[0],
            next: from(&v[1..]),
        }))
    }
    #[test]
    fn it_works() {
        assert_eq!(
            Solution::delete_duplicates(from(&vec![1, 1, 2])),
            from(&vec![1, 2])
        );
        assert_eq!(
            Solution::delete_duplicates(from(&vec![1, 1, 2, 3, 3])),
            from(&vec![1, 2, 3])
        );
    }
}
