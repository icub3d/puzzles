pub fn running_sum(nums: Vec<i32>) -> Vec<i32> {
    let mut sum = 0;
    let mut result = vec![];
    for n in nums.iter() {
        sum += n;
        result.push(sum);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(running_sum(vec![1, 2, 3, 4]), vec![1, 3, 6, 10]);
        assert_eq!(running_sum(vec![1, 1, 1, 1, 1]), vec![1, 2, 3, 4, 5]);
        assert_eq!(running_sum(vec![3, 1, 2, 10, 1]), vec![3, 4, 6, 16, 17]);
    }
}
