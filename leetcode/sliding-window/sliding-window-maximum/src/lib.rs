pub struct Solution;

impl Solution {
    pub fn max_sliding_window_naive(nums: Vec<i32>, k: i32) -> Vec<i32> {
        let mut ans = vec![];
        for window in nums.windows(k as usize) {
            ans.push(*window.iter().max().unwrap());
        }
        ans
    }

    pub fn max_sliding_window(nums: Vec<i32>, k: i32) -> Vec<i32> {
        use std::collections::VecDeque;

        let k = k as usize;

        // We can solve these patterns immediately.
        if nums.len() * k == 0 {
            return vec![];
        } else if k == 1 {
            return nums;
        }

        // The sliding window has the effect of removing the first
        // element and adding a new element. We can use a Deque here
        // to push and pop on both sides. We will pop from the front
        // to update the window and push new elements on the back.  We
        // can also remove any elements in the queue smaller than our
        // current position because they'll never be the largest in
        // that window.

        // Initialize the deque with our first window and our answer
        // with the max value.
        let mut window = VecDeque::new();
        let mut max = 0;
        for i in 0..k {
            while !window.is_empty() && nums[*window.back().unwrap()] < nums[i] {
                window.pop_back();
            }
            window.push_back(i);
            if nums[i] > nums[max] {
                max = i;
            }
        }
        let mut ans = vec![nums[max]];

        // Now we can iterate through the rest of the array removing and adding as we go.
        for i in k..nums.len() {
            // Adjust our sliding window if we've reached the limit.
            if !window.is_empty() && *window.front().unwrap() == i - k {
                window.pop_front();
            }

            // We don't need to keep smaller elements in our window as
            // they won't be candidates.
            while !window.is_empty() && nums[*window.back().unwrap()] < nums[i] {
                window.pop_back();
            }

            // Add the new element to the window.
            window.push_back(i);

            // Our answer to this window is the first element in the deque.
            ans.push(nums[*window.front().unwrap()]);
        }
        ans
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::max_sliding_window(vec![1, 3, -1, -3, 5, 3, 6, 7], 3),
            vec![3, 3, 5, 5, 6, 7]
        );
        assert_eq!(Solution::max_sliding_window(vec![1], 1), vec![1]);
        assert_eq!(Solution::max_sliding_window(vec![1, -1], 1), vec![1, -1]);
        assert_eq!(Solution::max_sliding_window(vec![9, 11], 2), vec![11]);
        assert_eq!(Solution::max_sliding_window(vec![4, -2], 2), vec![4]);
    }

    #[test]
    fn naive() {
        assert_eq!(
            Solution::max_sliding_window_naive(vec![1, 3, -1, -3, 5, 3, 6, 7], 3),
            vec![3, 3, 5, 5, 6, 7]
        );
        assert_eq!(Solution::max_sliding_window_naive(vec![1], 1), vec![1]);
        assert_eq!(
            Solution::max_sliding_window_naive(vec![1, -1], 1),
            vec![1, -1]
        );
        assert_eq!(Solution::max_sliding_window_naive(vec![9, 11], 2), vec![11]);
        assert_eq!(Solution::max_sliding_window_naive(vec![4, -2], 2), vec![4]);
    }
}
