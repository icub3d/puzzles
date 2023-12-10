#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

pub struct Solution;

impl Solution {
    pub fn remove_nth_from_end(head: Option<Box<ListNode>>, n: i32) -> Option<Box<ListNode>> {
        let mut dummy = Box::new(ListNode { val: 0, next: head });
        let mut cur = dummy.clone();
        for _ in 0..n {
            cur = cur.next.unwrap();
        }

        let mut nth = dummy.as_mut();
        while let Some(l) = cur.next {
            cur = l;
            nth = nth.next.as_mut().unwrap();
        }

        nth.next = nth.next.as_mut().unwrap().next.clone();

        dummy.next
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new(vv: &[i32]) -> Option<Box<ListNode>> {
        match vv.len() {
            0 => None,
            _ => Some(Box::new(ListNode {
                val: vv[0],
                next: new(&vv[1..]),
            })),
        }
    }

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::remove_nth_from_end(new(&vec![1, 2, 3, 4, 5]), 2),
            new(&vec![1, 2, 3, 5])
        );
        assert_eq!(
            Solution::remove_nth_from_end(new(&vec![1]), 1),
            new(&vec![])
        );
        assert_eq!(
            Solution::remove_nth_from_end(new(&vec![1, 2]), 1),
            new(&vec![1])
        );
        assert_eq!(
            Solution::remove_nth_from_end(new(&vec![1, 2]), 2),
            new(&vec![2])
        );
    }
}
