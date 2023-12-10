pub struct Solution;

impl Solution {
    pub fn longest_obstacle_course_at_each_position(obstacles: Vec<i32>) -> Vec<i32> {
        let mut dp = vec![0; obstacles.len()];
        let mut stack = vec![];
        for (i, &obstacle) in obstacles.iter().enumerate() {
            let mut left = 0;
            let mut right = stack.len();
            while left < right {
                let mid = left + (right - left) / 2;
                if stack[mid] <= obstacle {
                    left = mid + 1;
                } else {
                    right = mid;
                }
            }
            if left == stack.len() {
                stack.push(obstacle);
            } else {
                stack[left] = obstacle;
            }
            dp[i] = left as i32 + 1;
        }
        dp
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let obstacles = vec![1, 2, 3, 2];
        let result = Solution::longest_obstacle_course_at_each_position(obstacles);
        assert_eq!(result, vec![1, 2, 3, 3]);

        let obstacles = vec![2, 2, 1];
        let result = Solution::longest_obstacle_course_at_each_position(obstacles);
        assert_eq!(result, vec![1, 2, 1]);

        let obstacles = vec![3, 1, 5, 6, 4, 2];
        let result = Solution::longest_obstacle_course_at_each_position(obstacles);
        assert_eq!(result, vec![1, 1, 2, 3, 2, 2]);
    }
}
