pub struct Solution;

impl Solution {
    pub fn search(nums: Vec<i32>, target: i32) -> i32 {
        let mut left: i32 = 0;
        let mut right: i32 = (nums.len() - 1) as i32;
        while left <= right {
            let mid = (left + right) / 2;
            match target.cmp(&nums[mid as usize]) {
                std::cmp::Ordering::Equal => return mid,
                std::cmp::Ordering::Greater => left = mid + 1,
                std::cmp::Ordering::Less => right = mid - 1,
            };
        }
        -1
    }
}

#[cfg(test)]
mod tests {
    use super::Solution;

    #[test]
    fn it_works() {
        assert_eq!(Solution::search(vec![-1, 0, 3, 5, 9, 12], 9), 4);
        assert_eq!(Solution::search(vec![-1, 0, 3, 5, 9, 12], 2), -1);
        assert_eq!(Solution::search(vec![5], -5), -1);
        assert_eq!(Solution::search(vec![5], 5), 0);
        assert_eq!(Solution::search(vec![2, 5], 0), -1);
    }
}
