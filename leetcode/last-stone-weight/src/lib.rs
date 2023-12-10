pub struct Solution;

impl Solution {
    pub fn last_stone_weight(stones: Vec<i32>) -> i32 {
        use std::collections::BinaryHeap;
        let mut h = BinaryHeap::from(stones);
        loop {
            if h.len() == 0 {
                return 0;
            }
            if h.len() == 1 {
                return h.pop().unwrap();
            }

            let (l, r) = (h.pop().unwrap(), h.pop().unwrap());
            if l == r {
                continue;
            }
            h.push((l - r).abs());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(Solution::last_stone_weight(vec![2, 7, 4, 1, 8, 1]), 1);
        assert_eq!(Solution::last_stone_weight(vec![1]), 1);
    }
}
