pub fn pivot_index(nums: Vec<i32>) -> i32 {
    let mut right: i32 = nums.iter().sum();
    let mut left: i32 = 0;
    for (i, num) in nums.iter().enumerate() {
        right -= num;
        if left == right {
            return i as i32;
        }
        left += num;
    }
    -1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(pivot_index(vec![1, 7, 3, 6, 5, 6]), 3);
        assert_eq!(pivot_index(vec![1, 2, 3]), -1);
        assert_eq!(pivot_index(vec![2, 1, -1]), 0);
    }
}
