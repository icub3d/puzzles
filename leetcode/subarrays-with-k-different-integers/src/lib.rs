use std::collections::HashMap;

pub fn subarrays_with_k_distinct(nums: Vec<i32>, k: i32) -> i32 {
    count_at_most_k(&nums, k) - count_at_most_k(&nums, k - 1)
}

fn count_at_most_k(nums: &Vec<i32>, k: i32) -> i32 {
    let mut count = 0;
    let k = k as usize;
    let mut integers = HashMap::new();
    let mut left = 0;
    for right in 0..nums.len() {
        let num = nums[right];
        integers
            .entry(num)
            .and_modify(|count| *count += 1)
            .or_insert(1);
        while left <= right && integers.len() > k {
            let left_count = integers
                .entry(nums[left])
                .and_modify(|count| *count -= 1)
                .or_default();
            if *left_count == 0 {
                integers.remove(&nums[left]);
            }
            left += 1;
        }
        count += right - left + 1;
    }

    count as i32
}

#[cfg(test)]
mod tests {
    use super::subarrays_with_k_distinct;

    #[test]
    fn it_works() {
        assert_eq!(subarrays_with_k_distinct(vec![1, 2, 1, 2, 3], 2), 7);
        assert_eq!(subarrays_with_k_distinct(vec![1, 2, 1, 3, 4], 3), 3);
    }
}
