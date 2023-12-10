pub struct Solution;

impl Solution {
    pub fn find_missing_ranges(nums: Vec<i32>, lower: i32, upper: i32) -> Vec<Vec<i32>> {
        let mut r = vec![];

        if nums.len() == 0 {
            return vec![vec![lower, upper]];
        }

        // First compare lower to nums[0]
        if nums[0] != lower {
            r.push(vec![lower, nums[0] - 1]);
        }

        // Now we just need to find gaps in nums
        for i in 0..nums.len() - 1 {
            if nums[i] + 1 != nums[i + 1] {
                r.push(vec![nums[i] + 1, nums[i + 1] - 1]);
            }
        }

        // Finally compare edn to upper
        if nums[nums.len() - 1] != upper {
            r.push(vec![nums[nums.len() - 1] + 1, upper]);
        }

        r
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(
            Solution::find_missing_ranges(vec![0, 1, 3, 50, 75], 0, 99),
            vec![vec![2, 2], vec![4, 49], vec![51, 74], vec![76, 99]]
        );
        assert_eq!(
            Solution::find_missing_ranges(vec![], 1, 1),
            vec![vec![1, 1]]
        );
        assert_eq!(
            Solution::find_missing_ranges(vec![], -3, -1),
            vec![vec![-3, -1]]
        );
        let v: Vec<Vec<i32>> = Vec::new();
        assert_eq!(Solution::find_missing_ranges(vec![-1], -1, -1), v);
        assert_eq!(
            Solution::find_missing_ranges(vec![-1], -2, -1),
            vec![vec![-2, -2]]
        );
    }
}
