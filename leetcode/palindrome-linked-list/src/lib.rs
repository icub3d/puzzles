#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

pub struct Solution;

impl Solution {
    pub fn is_palindrome(mut head: Option<Box<ListNode>>) -> bool {
        let mut vv = vec![];
        while let Some(node) = head {
            vv.push(node.val);
            head = node.next;
        }

        vv.iter().eq(vv.iter().rev())
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
        assert_eq!(Solution::is_palindrome(from(&vec![1, 2, 2, 1])), true);
        assert_eq!(Solution::is_palindrome(from(&vec![1, 2, 1])), true);
        assert_eq!(Solution::is_palindrome(from(&vec![2, 1])), false);
    }
}
