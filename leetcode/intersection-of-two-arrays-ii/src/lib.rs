pub struct Solution;
use std::collections::hash_map::Entry;

use std::collections::HashMap;
impl Solution {
    pub fn intersect(nums1: Vec<i32>, nums2: Vec<i32>) -> Vec<i32> {
        let mut m = HashMap::new();
        for n in nums1.iter() {
            m.entry(n).and_modify(|v| *v += 1).or_insert(1);
        }

        let mut r = vec![];
        for n in nums2.iter() {
            match m.entry(n) {
                Entry::Occupied(mut e) => {
                    if *e.get() == 1 {
                        e.remove();
                    } else {
                        *e.get_mut() -= 1;
                    }
                    r.push(*n);
                }
                _ => (),
            };
        }
        r
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::intersect(vec![1, 2, 2, 1], vec![2, 2]),
            vec![2, 2]
        );
        assert_eq!(
            Solution::intersect(vec![4, 9, 5], vec![9, 4, 9, 8, 4]),
            vec![4, 9]
        );
    }
}
