pub struct Solution;

impl Solution {
    pub fn is_majority_element(nums: Vec<i32>, target: i32) -> bool {
        // It is sorted, so we can bsearch to find one and then we
        // check left and right until we've counted all of them.
        let idx = match nums.binary_search(&target) {
            Ok(x) => x,
            Err(_) => return false, // If not found, then it's def false.
        };

        let mut left = idx;
        while left > 0 && nums[left - 1] == target {
            left -= 1;
        }
        let mut right = idx;
        while right < nums.len() && nums[right] == target {
            right += 1;
        }

        (right - left) > nums.len() / 2
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::is_majority_element(vec![2, 4, 5, 5, 5, 5, 5, 6, 6], 5),
            true
        );
        assert_eq!(
            Solution::is_majority_element(vec![10, 100, 101, 101], 101),
            false
        );
    }
}
