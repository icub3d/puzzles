#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

pub struct Solution;

impl Solution {
    pub fn odd_even_list(mut head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {}
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
        assert_eq!(Solution::odd_even_list(from(&vec![1])), from(&vec![1]));
        assert_eq!(
            Solution::odd_even_list(from(&vec![1, 2])),
            from(&vec![1, 2])
        );
        assert_eq!(
            Solution::odd_even_list(from(&vec![1, 2, 3, 4, 5])),
            from(&vec![1, 3, 5, 2, 4])
        );
    }
}
