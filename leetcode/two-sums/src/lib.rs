pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32> {
    use std::collections::HashMap;
    let mut known: HashMap<i32, i32> = HashMap::new();
    for (i, num) in nums.iter().enumerate() {
        if let Some(j) = known.get(&(target - num)) {
            return vec![i as i32, *j];
        }
        known.insert(*num, i as i32);
    }
    vec![0, 0]
}

#[cfg(test)]
mod tests {
    use super::two_sum;
    #[test]
    fn it_works() {
        assert_eq!(two_sum(vec![2, 7, 11, 15], 9), vec![1, 0]);
        assert_eq!(two_sum(vec![3, 2, 4], 6), vec![2, 1]);
        assert_eq!(two_sum(vec![3, 3], 6), vec![1, 0]);
    }
}
