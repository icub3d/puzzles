pub fn max_sub_array(nums: Vec<i32>) -> i32 {
    // Kadane's Algorithm.
    // https://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane's_algorithm
    let (mut best, mut cur) = (nums[0], nums[0]);
    for num in nums.iter().skip(1) {
        cur = *num + 0.max(cur);
        best = best.max(cur);
    }
    best
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn it_works() {
        assert_eq!(max_sub_array(vec![-2, 1, -3, 4, -1, 2, 1, -5, 4]), 6);
        assert_eq!(max_sub_array(vec![1]), 1);
        assert_eq!(max_sub_array(vec![5, 4, -1, 7, 8]), 23)
    }
}
