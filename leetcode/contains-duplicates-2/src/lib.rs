pub fn contains_nearby_duplicate(nums: Vec<i32>, k: i32) -> bool {
    use std::collections::HashSet;
    let k = k as usize;
    if nums.len() < 2 || k < 1 {
        return false;
    }
    let mut left = 0;
    let mut current_window = HashSet::new();
    current_window.insert(nums[0]);
    for right in 1..nums.len() {
        if current_window.contains(&nums[right]) {
            return true;
        }
        current_window.insert(nums[right]);
        if current_window.len() > k {
            current_window.remove(&nums[left]);
            left += 1;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::contains_nearby_duplicate;

    #[test]
    fn it_works() {
        assert_eq!(contains_nearby_duplicate(vec![1, 2, 3, 1], 3), true);
        assert_eq!(contains_nearby_duplicate(vec![1, 0, 1, 1], 1), true);
        assert_eq!(contains_nearby_duplicate(vec![1, 2, 3, 1, 2, 3], 2), false);
        assert_eq!(contains_nearby_duplicate(vec![1, 2, 3, 1], 0), false);
    }
}
