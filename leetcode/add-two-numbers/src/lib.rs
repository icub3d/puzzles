#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

pub struct Solution;

impl Solution {
    pub fn add_two_numbers(
        l1: Option<Box<ListNode>>,
        l2: Option<Box<ListNode>>,
    ) -> Option<Box<ListNode>> {
        let mut head = ListNode { val: 0, next: None };
        let mut cur = &mut head;
        let mut carry = 0;
        let mut l1 = l1;
        let mut l2 = l2;
        while !l1.is_none() || !l2.is_none() {
            let v = match (&l1, &l2) {
                (None, Some(r)) => r.val + carry,
                (Some(l), None) => l.val + carry,
                (Some(l), Some(r)) => l.val + r.val + carry,
                _ => panic!("not good"),
            };
            carry = v / 10;
            cur.next = Some(Box::new(ListNode {
                val: v % 10,
                next: None,
            }));
            cur = cur.next.as_mut().unwrap();
            if let Some(l) = l1 {
                l1 = l.next;
            }
            if let Some(r) = l2 {
                l2 = r.next;
            }
        }
        if carry > 0 {
            cur.next = Some(Box::new(ListNode {
                val: carry,
                next: None,
            }));
        }
        head.next
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
            Solution::add_two_numbers(from(&vec![0]), from(&vec![0])),
            from(&vec![0])
        );
        assert_eq!(
            Solution::add_two_numbers(from(&vec![2, 4, 3]), from(&vec![5, 6, 4])),
            from(&vec![7, 0, 8])
        );
        assert_eq!(
            Solution::add_two_numbers(from(&vec![9, 9, 9, 9, 9, 9, 9]), from(&vec![9, 9, 9, 9])),
            from(&vec![8, 9, 9, 9, 0, 0, 0, 1])
        );
    }
}
